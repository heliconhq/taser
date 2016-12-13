-module(taser_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% Supervisor callbacks

-export([init/1]).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [
    ],
    {ok, {SupFlags, ChildSpecs}}.
