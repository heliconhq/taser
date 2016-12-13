-module(taser_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% Application callbacks

start(_StartType, _StartArgs) ->
    taser_sup:start_link().

stop(_State) ->
    ok.
