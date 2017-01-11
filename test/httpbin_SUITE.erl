-module(httpbin_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([get/1]).
-export([redirect/1]).
-export([post_form/1]).
-export([basic_auth/1]).

%% Callbacks

all() ->
    [
        get,
        redirect,
        post_form,
        basic_auth
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

post_form(_Config) ->
    {ok, 200, _Headers1, Body1} = taser:post_form("http://httpbin.org/post", #{ a => b }),
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
