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

-module(taser_gun).

-export([request/4]).

-record(state, {
        connect_timeout,
        response_timeout,
        follow_redirects,
        max_redirects,

        conn_pid,
        mref,
        streamref,

        protocol,
        auth,
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

request(Method, {Protocol, Auth, Hostname, Port, Path, Query, CombinedPath},
        Headers, Opts) ->
    GunOpts = gun_opts(Protocol, Opts),
    case taser_dns:lookup(Hostname) of
        {ok, IP} ->
            NewHeaders = [{<<"host">>, [list_to_binary(Hostname), $:, integer_to_binary(Port)]}|Headers],
            {ok, ConnPid} = gun:open(IP, Port, GunOpts),
            MRef = erlang:monitor(process, ConnPid),
            State = initial_state(ConnPid, MRef, Protocol, Auth, Hostname, Port,
                                  Method, Path, Query, CombinedPath,
                                  NewHeaders, Opts),
            handle_connection(State, prepare_data(Opts));
        Error ->
            Error
    end.

handle_connection(State = #state{ conn_pid = ConnPid, mref = MRef, method =
                                  Method, combined_path = Path,
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

maybe_follow_redirect(_Finished, #state{ conn_pid = ConnPid, max_redirects =
                                         MaxRedirects, headers = Headers,
                                         connect_timeout = ConnTimeout,
                                         response_timeout = RespTimeout,
                                         request_headers = RequestHeaders,
                                         follow_redirects = true, protocol =
                                         Protocol, hostname = Hostname, port =
                                         Port, path = Path, mref = MRef,
                                         status = Status })
  when Status >= 301 andalso Status =< 303 ->
    %% TODO: Handle 307?
    shutdown(ConnPid, MRef),
    case lists:keyfind(<<"location">>, 1, Headers) of
        false ->
            {error, redirect_without_location};
        {<<"location">>, Location} ->
            %% TODO: Reset response timeout?
            if
                MaxRedirects =< 0 ->
                    {error, max_redirects};
                true ->
                    AbsLoc = taser_utils:absolute_location(Location, Protocol,
                                                           Hostname, Port,
                                                           Path),
                    RequestHeaders2 = lists:keydelete(<<"host">>, 1,
                                                      RequestHeaders),
                    taser:request(get, AbsLoc, RequestHeaders2, #{
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

maybe_deflate(Body, Headers) ->
    case lists:keyfind(<<"content-encoding">>, 1, Headers) of
        {_, <<"gzip">>} ->
            zlib:gunzip(Body);
        _ ->
            Body
    end.

gun_opts(Protocol, _Opts) ->
    #{
        transport => transport(Protocol),
        transport_opts => transport_opts(transport(Protocol)) 
    }.

transport_opts(ssl) ->
    [{versions, ['tlsv1.2']}];
transport_opts(_) ->
    [].

initial_state(ConnPid, MRef, Protocol, Auth, Hostname, Port, Method, Path,
              Query, CombinedPath, RequestHeaders, Opts) ->
    #state { 
        conn_pid = ConnPid,
        mref = MRef,

        connect_timeout = maps:get(connect_timeout, Opts, 5000),
        response_timeout = maps:get(response_timeout, Opts, 5000),
        follow_redirects = maps:get(follow_redirects, Opts, false),
        max_redirects = maps:get(max_redirects, Opts, 5),

        protocol = Protocol,
        auth = Auth,
        hostname = Hostname,
        port = Port,
        method = method(Method),
        path = Path,
        query = Query,
        combined_path = CombinedPath,

        request_headers = maybe_add_auth(Auth, RequestHeaders)
    }.

maybe_add_auth([], Headers) ->
    Headers;

maybe_add_auth(Auth, Headers) ->
    Auth64 = base64:encode(Auth),
    [{<<"Authorization">>, <<"Basic ", Auth64/binary>>}|Headers].

prepare_data(#{ data := Data }) ->
    Data;
prepare_data(#{ form := Form }) ->
    taser_utils:urlencode_pairs(Form);
prepare_data(_) ->
    <<>>.

transport(http) ->
    tcp;
transport(https) ->
    ssl.

method(Method) when is_binary(Method) ->
    binary_to_list(Method);
method(Method) when is_list(Method) ->
    string:to_upper(Method);
method(put) ->
    "PUT";
method(delete) ->
    "DELETE";
method(patch) ->
    "PATCH";
method(options) ->
    "OPTIONS";
method(get) ->
    "GET";
method(post) ->
    "POST";
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
