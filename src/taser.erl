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

-export([delete/1]).
-export([delete/2]).
-export([get/1]).
-export([get/2]).
-export([head/1]).
-export([head/2]).
-export([options/1]).
-export([options/2]).
-export([patch/2]).
-export([patch/3]).
-export([post/2]).
-export([post/3]).
-export([post_form/2]).
-export([post_form/3]).
-export([put/2]).
-export([put/3]).
-export([put_form/2]).
-export([put_form/3]).

start() ->
    application:ensure_all_started(taser).

stop() ->
    application:stop(taser).

delete(URL) ->
    delete(URL, []).

delete(URL, Headers) ->
    request(delete, URL, Headers).

get(URL) ->
    get(URL, []).

get(URL, Headers) ->
    request(get, URL, Headers).

head(URL) ->
    head(URL, []).

head(URL, Headers) ->
    request(head, URL, Headers).

options(URL) ->
    options(URL, []).

options(URL, Headers) ->
    request(options, URL, Headers).

post(URL, Data) ->
    post(URL, Data, []).

post(URL, Data, Headers) ->
    request(post, URL, Headers, #{ data => Data }).

post_form(URL, Form) ->
    post_form(URL, Form, []).

post_form(URL, Form, Headers) ->
    Header = {"Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"},
    NewHeaders = [Header|Headers],
    request(post, URL, NewHeaders, #{ form => Form }).

patch(URL, Data) ->
    patch(URL, Data, []).

patch(URL, Data, Headers) ->
    request(patch, URL, Headers, #{ data => Data }).

put(URL, Data) ->
    put(URL, Data, []).

put(URL, Data, Headers) ->
    request(put, URL, Headers, #{ data => Data }).

put_form(URL, Form) ->
    put_form(URL, Form, []).

put_form(URL, Form, Headers) ->
    Header = {"Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"},
    NewHeaders = [Header|Headers],
    request(put, URL, NewHeaders, #{ form => Form }).

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
