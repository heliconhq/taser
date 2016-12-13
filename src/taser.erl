%% Copyright (c) 2016, Gustaf Sjöberg <gs@trell.se>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(taser).

-export([start/0]).
-export([stop/0]).

-export([request/2]).
-export([request/3]).
-export([request/4]).

-export([get/1]).
-export([get/2]).
-export([post/2]).
-export([post/3]).
-export([put/2]).
-export([put/3]).

-record(state, {
        connect_timeout,
        response_timeout,
        follow_redirects,
        max_redirects,

        conn_pid,
        mref,
        streamref,

        protocol,
        hostname,
        port,
        method,
        path,
        query,
        combined_path,
        request_headers,

        status,
        headers
    }).

start() ->
    application:ensure_all_started(taser).

stop() ->
    application:stop(taser).

get(URL) ->
    request(get, URL, []).

get(URL, Headers) ->
    request(get, URL, Headers).

post(URL, Data) ->
    post(URL, Data, []).

post(URL, Data, Headers) ->
    request(post, URL, Headers, #{ data => Data }).

put(URL, Data) ->
    put(URL, Data, []).

put(URL, Data, Headers) ->
    request(post, URL, Headers, #{ data => Data }).

request(Method, URL) ->
    request(Method, URL, [], #{}).

request(Method, URL, Headers) when is_list(Headers) ->
    request(Method, URL, Headers, #{});

request(Method, URL, Opts) when is_map(Opts) ->
    request(Method, URL, [], Opts).

request(Method, URL, Headers, Opts) when is_binary(URL) ->
    request(Method, binary_to_list(URL), Headers, Opts);

request(Method, URL, Headers, Opts) ->
    lager:info("Requesting URL: ~p", [URL]),
    case parse_uri(URL) of
        {ok, {Protocol, Auth, Hostname, Port , Path, Query, CombinedPath}} ->
            raw_request(Method, {Protocol, Auth, Hostname, Port, Path, Query,
                                 CombinedPath}, Headers, Opts);
        Error ->
            Error
    end.

raw_request(Method, {Protocol, _Auth, Hostname, Port , Path, Query,
                     CombinedPath}, Headers, Opts) ->
    GunOpts = gun_opts(Protocol, Opts),
    {ok, ConnPid} = gun:open(Hostname, Port, GunOpts),
    lager:info("ConnPid: ~p", [ConnPid]),
    MRef = erlang:monitor(process, ConnPid),
    State = initial_state(ConnPid, MRef, Protocol, Hostname, Port, Method,
                          Path, Query, CombinedPath, Headers, Opts),
    handle_connection(State, maps:get(data, Opts, <<>>)).

handle_connection(State = #state{ conn_pid = ConnPid, mref = MRef, method =
                                  Method, path = Path,
                                  request_headers = RequestHeaders,
                                  connect_timeout = ConnTimeout}, Data) ->
    receive
        {'DOWN', MRef, process, ConnPid, Reason} ->
            exit(Reason);
        {gun_up, ConnPid, _Transport} ->
            StreamRef = case Data of 
                <<>> ->
                    gun:request(ConnPid, Method, Path, RequestHeaders);
                _Data ->
                    gun:request(ConnPid, Method, Path, RequestHeaders, Data)
            end,
            handle_response(State#state{ streamref = StreamRef });
        {gun_down, ConnPid, _Transport, Reason, _StreamRefs, _StreamRefs} ->
            close(ConnPid, MRef, Reason);
        Other ->
            close(ConnPid, MRef, {conn, Other})
    after ConnTimeout ->
        close(ConnPid, MRef, connect_timeout)
    end.

handle_response(State = #state{ conn_pid = ConnPid, mref = MRef,
                                streamref = StreamRef,
                                response_timeout = RespTimeout}) ->
    StartedAt = erlang:system_time(millisecond),
    receive
        {'DOWN', MRef, process, ConnPid, Reason} ->
            exit(Reason);
        {gun_response, ConnPid, StreamRef, Finished, Status, Headers} ->
            TimeDiff = erlang:system_time(millisecond) - StartedAt,
            maybe_follow_redirect(Finished, State#state{
                status = Status,
                headers = Headers,
                response_timeout = RespTimeout - TimeDiff
            });
        {gun_error, ConnPid, _StreamRef, Reason} ->
            close(ConnPid, MRef, Reason);
        {gun_error, ConnPid, Reason} ->
            close(ConnPid, MRef, Reason);
        Other ->
            close(ConnPid, MRef, {response, Other})
    after RespTimeout ->
        close(ConnPid, MRef, response_timeout)
    end.

maybe_follow_redirect(_Finished, State = #state{ conn_pid = ConnPid,
                                                 max_redirects = MaxRedirects,
                                                 headers = Headers,
                                                 connect_timeout = ConnTimeout,
                                                 response_timeout = RespTimeout,
                                                 request_headers = RequestHeaders,
                                                 follow_redirects = true,
                                                 mref = MRef, status = Status })
  when MaxRedirects > 0 andalso Status >= 301 andalso Status =< 303 ->
    %% TODO: Handle 307?
    shutdown(ConnPid, MRef),
    case lists:keyfind(<<"location">>, 1, Headers) of
        false ->
            {error, redirect_without_location};
        {<<"location">>, Location} ->
            lager:info("Location: ~p", [Location]),
            %% TODO: Reset response timeout?
            lager:info("Max redirects: ~p", [MaxRedirects]),
            if
                MaxRedirects - 1 =< 0 ->
                    {error, max_redirects};
                true ->
                    request(get, absolute_location(Location, State),
                            RequestHeaders, #{
                        connect_timeout => ConnTimeout,
                        response_timeout => RespTimeout,
                        follow_redirects => true,
                        max_redirects => MaxRedirects - 1
                    })
            end
    end;

maybe_follow_redirect(fin, #state{ conn_pid = ConnPid, mref = MRef,
                                   status = Status, headers = Headers }) ->
    shutdown(ConnPid, MRef),
    {ok, Status, Headers, <<>>};

maybe_follow_redirect(nofin, State) ->
    handle_body(State).

handle_body(State) ->
    handle_body(State, <<>>).

handle_body(State = #state{ conn_pid = ConnPid, mref = MRef, headers =
                            Headers, status = Status, streamref = StreamRef,
                            response_timeout = RespTimeout }, Buf) ->
    StartedAt = erlang:system_time(millisecond),
    receive
        {'DOWN', MRef, process, ConnPid, Reason} ->
            exit(Reason);
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            TimeDiff = erlang:system_time(millisecond) - StartedAt,
            handle_body(State#state{
                response_timeout = RespTimeout - TimeDiff
            }, <<Buf/binary, Data/binary>>);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            shutdown(ConnPid, MRef),
            Body = maybe_deflate(<<Buf/binary, Data/binary>>, Headers),
            {ok, Status, Headers, Body};
        {gun_error, ConnPid, StreamRef, Reason} ->
            close(ConnPid, MRef, Reason);
        {gun_error, ConnPid, Reason} ->
            close(ConnPid, MRef, Reason);
        Other ->
            close(ConnPid, MRef, {body, Other})
    after RespTimeout ->
        close(ConnPid, MRef, body_timeout)
    end.

parse_uri(URI) ->
    case http_uri:parse(URI) of
        {error, no_scheme} ->
            parse_uri("http://" ++ URI);
        {ok, {Protocol, Auth, Hostname, Port , Path, Query}} ->
            {ok, {Protocol, Auth, Hostname, Port, Path, Query,
                  Path ++ escape_query(Query)}};
        Error ->
            Error
    end.

maybe_deflate(Body, Headers) ->
    case lists:keyfind(<<"content-encoding">>, 1, Headers) of
        {_, <<"gzip">>} ->
            zlib:gunzip(Body);
        _ ->
            Body
    end.

gun_opts(Protocol, _Opts) ->
    #{
        transport => transport(Protocol)
    }.

initial_state(ConnPid, MRef, Protocol, Hostname, Port, Method, Path, Query,
              CombinedPath, RequestHeaders, Opts) ->
    #state { 
        conn_pid = ConnPid,
        mref = MRef,

        connect_timeout = maps:get(connect_timeout, Opts, 5000),
        response_timeout = maps:get(response_timeout, Opts, 5000),
        follow_redirects = maps:get(follow_redirects, Opts, false),
        max_redirects = maps:get(max_redirects, Opts, 5),

        protocol = Protocol,
        hostname = Hostname,
        port = Port,
        method = method(Method),
        path = Path,
        query = Query,
        combined_path = CombinedPath,

        request_headers = RequestHeaders
    }.

transport(http) ->
    tcp;
transport(https) ->
    ssl.

method(Method) when is_binary(Method) ->
    binary_to_list(Method);
method(Method) when is_list(Method) ->
    string:to_upper(Method);
method(get) ->
    "GET";
method(post) ->
    "POST";
method(delete) ->
    "DELETE";
method(head) ->
    "HEAD".

shutdown(ConnPid, MRef) ->
    erlang:demonitor(MRef, [flush]),
    gun:close(ConnPid),
    gun:flush(ConnPid).

close(ConnPid, MRef, Reason) ->
    erlang:demonitor(MRef, [flush]),
    gun:close(ConnPid),
    gun:flush(ConnPid),
    {error, Reason}.

escape_query("") ->
    "";
escape_query("?" ++ Query) ->
    Tokens = string:tokens(Query, "&"),
    EscapedQuery = escape_tokens(Tokens),
    "?" ++ EscapedQuery.

escape_tokens(Tokens) ->
    escape_tokens(Tokens, []).
escape_tokens([Token], Acc) ->
    Acc ++ escape_token(Token);
escape_tokens([Token|Rest], Acc) ->
    escape_tokens(Rest, Acc ++ escape_token(Token) ++ "&").

escape_token(Token) ->
    case string:tokens(Token, "=") of
        [Key, Value] ->
            edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value);
        [Value]  ->
            edoc_lib:escape_uri(Value)
    end.

%% TODO: Preserve auth details during redirects?
absolute_location(<<"http://", _/binary>> = Location, _State) ->
    Location;
absolute_location(<<"https://", _/binary>> = Location, _State) ->
    Location;
absolute_location(<<"/", _/binary>> = Location, #state{ protocol = Protocol,
                                                        hostname = Hostname,
                                                        port = Port }) ->
    atom_to_list(Protocol) ++ "://" ++ Hostname ++ ":" ++
        integer_to_list(Port) ++ binary_to_list(Location);
absolute_location(Location, #state{ protocol = Protocol,
                                    hostname = Hostname,
                                    port = Port,
                                    path = Path }) ->
    %% We're assuming a location relative to the current path.
    atom_to_list(Protocol) ++ "://" ++ Hostname ++ ":" ++
        integer_to_list(Port) ++ Path ++ binary_to_list(Location).
