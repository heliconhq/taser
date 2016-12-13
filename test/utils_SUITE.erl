-module(utils_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([tokenizer/1]).
-export([urlencode_pairs/1]).

%% Callbacks

all() ->
    [
        tokenizer,
        urlencode_pairs
    ].

init_per_suite(Config) ->
    application:ensure_all_started(taser),
    Config.

end_per_suite(_Config) ->
    ok.

tokenizer(_Config) ->
    [{"a", "b"}, {"c", "d"}] = taser_utils:tokenize_querystring("a=b&c=d"),
    [{"a", "b"}, {"cd", ""}] = taser_utils:tokenize_querystring("a=b&cd"),
    [""] = taser_utils:tokenize_querystring("="),
    ok.

urlencode_pairs(_Config) ->
    "a=b&c=d" = taser_utils:urlencode_pairs([{a, b}, {c, d}]),
    "hello=%20world" = taser_utils:urlencode_pairs([{"hello", " world"}]),
    ok.
