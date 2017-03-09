%% Copyright (c) 2017, Gustaf Sjöberg <gs@trell.se>
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

-module(taser_dns).

-behaviour(gen_server).

-export([lookup/1]).
-export([start_link/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include_lib("kernel/include/inet.hrl").

-record(state, {
    active_bucket,
    inactive_bucket,
    sweep_counter = 0
}).

-define(SWEEP_INTERVAL, 60000 * 3).
-define(FULL_SWEEP_FREQUENCY, 6).

%% The current sweep implementation is pretty naive. A regular sweep will
%% destroy the active bucket incurring a double ets-lookup (which is fine). If
%% the value is found it will be placed in the active bucket again. A full
%% sweep runs every N sweeps and will evict _all_ values. This means that an
%% active system will get hit by a ton of lookups just after a full sweep. The
%% buckets could be divided into several partitions evicted on rotation, but
%% I'm wondering if that really is necessary.

%% External API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lookup(Hostname) ->
    {ok, Active, Inactive} = get_tables(),
    case cache_lookup(Hostname, Active, Inactive) of
        {active_hit, IP} ->
            {ok, IP};
        {inactive_hit, IP} ->
            ets:insert(Active, {Hostname, IP}),
            {ok, IP};
        miss ->
            case gethostbyname(Hostname) of
                {ok, IP} ->
                    ets:insert(Active, {Hostname, IP}),
                    {ok, IP};
                Error ->
                    Error
            end
    end.

%% Callbacks

init([]) ->
    inet_db:set_timeout(2000),
    inet_db:set_retry(2),
    TBL1 = ets:new(taser_host_dns_bucket_1, [ordered_set, named_table, public]),
    TBL2 = ets:new(taser_host_dns_bucket_2, [ordered_set, named_table, public]),
    State = #state{ active_bucket = TBL1, inactive_bucket = TBL2 },
    init_next_sweep(),
    {ok, State}.

handle_call(get_tables, _From, #state{ active_bucket = Active,
                                       inactive_bucket = Inactive } = State) ->
    {reply, {ok, Active, Inactive}, State};

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(sweep, #state{ sweep_counter = ?FULL_SWEEP_FREQUENCY,
                           active_bucket = Active,
                           inactive_bucket = Inactive } = State) ->
    ets:delete_all_objects(Active),
    ets:delete_all_objects(Inactive),
    NewState = State#state{ sweep_counter = 0 },
    init_next_sweep(),
    {noreply, NewState};

handle_info(sweep, #state{ sweep_counter = Counter, active_bucket = Active,
                           inactive_bucket = Inactive } = State) ->
    ets:delete_all_objects(Inactive),
    NewState = State#state{
        active_bucket = Inactive,
        inactive_bucket = Active,
        sweep_counter = Counter + 1
    },
    init_next_sweep(),
    {noreply, NewState};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Internal API

init_next_sweep() ->
    _ = erlang:send_after(?SWEEP_INTERVAL, self(), sweep),
    ok.

cache_lookup(Hostname, Active, Inactive) ->
    case ets:lookup(Active, Hostname) of
        [{Hostname, IP}] ->
            {active_hit, IP};
        [] ->
            case ets:lookup(Inactive, Hostname) of
                [{Hostname, IP}] ->
                    {inactive_hit, IP};
                [] ->
                    miss
            end
    end.

gethostbyname(Hostname) ->
    gethostbyname(Hostname, [inet_res, inet_gethost_native]).

gethostbyname(_Hostname, []) ->
    {error, nxdomain};

gethostbyname(Hostname, [Module|Modules]) ->
    case Module:gethostbyname(Hostname) of
        {ok, #hostent{ h_addr_list = [RawIP|_] }} ->
            IP = inet:ntoa(RawIP),
            {ok, IP};
        {error, formerr} ->
            {error, einval};
        {error, _} ->
            gethostbyname(Hostname, Modules)
    end.

get_tables() ->
    gen_server:call(?MODULE, get_tables).
