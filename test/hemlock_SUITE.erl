-module(hemlock_SUITE).

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
-export([status_codes/1]).
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
        status_codes,
        redirect
    ].

init_per_suite(Config) ->
    application:ensure_all_started(taser),
    application:ensure_all_started(hemlock),
    Config.

end_per_suite(_Config) ->
    application:stop(hemlock),
    ok.

%% Tests

get(_Config) ->
    {ok, 200, _Headers, _Body} = taser:get("http://127.0.0.1:5000/get"),
    {ok, 200, _Headers2, Body2} = taser:get("http://127.0.0.1:5000/get?a=b"),
    #{ <<"query">> := #{ <<"a">> := <<"b">> } } =
        jsx:decode(Body2, [return_maps]),
    ok.

head(_Config) ->
    {ok, 200, _Headers, _Body} = taser:head("http://127.0.0.1:5000/head"),
    ok.

options(_Config) ->
    {ok, 200, _Headers, _Body} = taser:options("http://127.0.0.1:5000/options"),
    ok.

put(_Config) ->
    {ok, 200, _Headers, Body} =
        taser:put("http://127.0.0.1:5000/put", <<"test">>),

        #{ <<"data">> := <<"test">> } =
            jsx:decode(Body, [return_maps]),
    ok.

delete(_Config) ->
    {ok, 200, _Headers, _Body} = taser:delete("http://127.0.0.1:5000/delete"),

    ok.

post(_Config) ->
    {ok, 200, _Headers, Body} =
        taser:post("http://127.0.0.1:5000/post", <<"test">>),

        #{ <<"data">> := <<"test">> } =
            jsx:decode(Body, [return_maps]),
    ok.

patch(_Config) ->
    {ok, 200, _Headers, Body} =
    taser:patch("http://127.0.0.1:5000/patch", <<"test">>),

        #{ <<"data">> := <<"test">> } =
            jsx:decode(Body, [return_maps]),
    ok.

post_form(_Config) ->
    {ok, 200, _Headers1, Body1} =
    taser:post_form("http://127.0.0.1:5000/post", #{ a => b }),
    #{ <<"form">> := #{ <<"a">> := <<"b">> } } =
        jsx:decode(Body1, [return_maps]),
    ok.

redirect(_Config) ->
    URL = "http://127.0.0.1:5000/redirect/3",
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
    {ok, 200, _Headers2, _Body2} =
    taser:get("http://user:passwd@127.0.0.1:5000/auth/user/passwd"),
    {ok, 401, _Headers, _Body} =
    taser:get("http://user:something@127.0.0.1:5000/auth/user/passwd"),
    ok.

status_codes(_Config) ->
    {ok, 100, _, _} = taser:get("http://127.0.0.1:5000/status/100"),
    {ok, 100, _, _} = taser:put("http://127.0.0.1:5000/status/100", <<>>),
    {ok, 100, _, _} = taser:post("http://127.0.0.1:5000/status/100", <<>>),
    {ok, 100, _, _} = taser:patch("http://127.0.0.1:5000/status/100", <<>>),
    {ok, 100, _, _} = taser:head("http://127.0.0.1:5000/status/100"),
    {ok, 100, _, _} = taser:options("http://127.0.0.1:5000/status/100"),
    {ok, 200, _, _} = taser:get("http://127.0.0.1:5000/status/200"),
    {ok, 200, _, _} = taser:put("http://127.0.0.1:5000/status/200", <<>>),
    {ok, 200, _, _} = taser:post("http://127.0.0.1:5000/status/200", <<>>),
    {ok, 200, _, _} = taser:patch("http://127.0.0.1:5000/status/200", <<>>),
    {ok, 200, _, _} = taser:head("http://127.0.0.1:5000/status/200"),
    {ok, 200, _, _} = taser:options("http://127.0.0.1:5000/status/200"),
    {ok, 300, _, _} = taser:get("http://127.0.0.1:5000/status/300"),
    {ok, 300, _, _} = taser:put("http://127.0.0.1:5000/status/300", <<>>),
    {ok, 300, _, _} = taser:post("http://127.0.0.1:5000/status/300", <<>>),
    {ok, 300, _, _} = taser:patch("http://127.0.0.1:5000/status/300", <<>>),
    {ok, 300, _, _} = taser:head("http://127.0.0.1:5000/status/300"),
    {ok, 300, _, _} = taser:options("http://127.0.0.1:5000/status/300"),
    {ok, 400, _, _} = taser:get("http://127.0.0.1:5000/status/400"),
    {ok, 400, _, _} = taser:put("http://127.0.0.1:5000/status/400", <<>>),
    {ok, 400, _, _} = taser:post("http://127.0.0.1:5000/status/400", <<>>),
    {ok, 400, _, _} = taser:patch("http://127.0.0.1:5000/status/400", <<>>),
    {ok, 400, _, _} = taser:head("http://127.0.0.1:5000/status/400"),
    {ok, 400, _, _} = taser:options("http://127.0.0.1:5000/status/400"),
    {ok, 500, _, _} = taser:get("http://127.0.0.1:5000/status/500"),
    {ok, 500, _, _} = taser:put("http://127.0.0.1:5000/status/500", <<>>),
    {ok, 500, _, _} = taser:post("http://127.0.0.1:5000/status/500", <<>>),
    {ok, 500, _, _} = taser:patch("http://127.0.0.1:5000/status/500", <<>>),
    {ok, 500, _, _} = taser:head("http://127.0.0.1:5000/status/500"),
    {ok, 500, _, _} = taser:options("http://127.0.0.1:5000/status/500"),
    {ok, 400, _, _} = taser:options("http://127.0.0.1:5000/status/abc"),
    ok.
