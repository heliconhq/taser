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
    case taser_utils:parse_uri(URL) of
        {ok, ParsedURL} ->
            taser_gun:request(Method, ParsedURL, Headers, Opts);
        Error ->
            Error
    end.

