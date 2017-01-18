-module(httpbin_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([basic_auth/1]).
-export([delete/1]).
-export([get/1]).
-export([head/1]).
-export([options/1]).
-export([patch/1]).
-export([post/1]).
-export([post_form/1]).
-export([put/1]).
-export([redirect/1]).

%% Callbacks

all() ->
    [
        basic_auth,
        delete,
        get,
        head,
        options,
        patch,
        post,
        post_form,
        put,
        redirect
    ].

init_per_suite(Config) ->
    application:ensure_all_started(taser),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests

get(_Config) ->
    {ok, 200, _Headers, _Body} = taser:get("http://httpbin.org/get"),
    {ok, 200, _Headers2, Body2} = taser:get("http://httpbin.org/get?a=b"),
    #{ <<"args">> := #{ <<"a">> := <<"b">> } } =
        jsx:decode(Body2, [return_maps]),
    ok.

head(_Config) ->
    {ok, 200, _Headers, _Body} = taser:head("http://httpbin.org/get"),
    ok.

options(_Config) ->
    {ok, 200, _Headers, _Body} = taser:head("http://httpbin.org/"),
    ok.

put(_Config) ->
    {ok, 200, _Headers, Body} =
        taser:put("http://httpbin.org/put", <<"test">>),

        #{ <<"data">> := <<"test">> } =
            jsx:decode(Body, [return_maps]),
    ok.

delete(_Config) ->
    {ok, 200, _Headers, _Body} =
        taser:delete("http://httpbin.org/delete"),

    ok.

post(_Config) ->
    {ok, 200, _Headers, Body} =
        taser:post("http://httpbin.org/post", <<"test">>),

        #{ <<"data">> := <<"test">> } =
            jsx:decode(Body, [return_maps]),
    ok.

patch(_Config) ->
    {ok, 200, _Headers, Body} =
        taser:patch("http://httpbin.org/patch", <<"test">>),

        #{ <<"data">> := <<"test">> } =
            jsx:decode(Body, [return_maps]),
    ok.

post_form(_Config) ->
    {ok, 200, _Headers1, Body1} =
        taser:post_form("http://httpbin.org/post", #{ a => b }),
    #{ <<"form">> := #{ <<"a">> := <<"b">> } } =
        jsx:decode(Body1, [return_maps]),
    ok.

redirect(_Config) ->
    URL = "http://httpbin.org/redirect/3",
    {ok, 200, _Headers, _Body} = taser:request(get, URL, #{
        follow_redirects => true,
        max_redirects => 3
    }),
    {error, max_redirects} = taser:request(get, URL, #{
        follow_redirects => true,
        max_redirects => 2
    }),
    ok.

basic_auth(_Config) ->
    {ok, 401, _Headers, _Body} =
        taser:get("http://user@httpbin.org/basic-auth/user/passwd"),
    {ok, 200, _Headers2, _Body2} =
        taser:get("http://user:passwd@httpbin.org/basic-auth/user/passwd"),
    ok.
