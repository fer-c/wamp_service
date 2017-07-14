%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================


-module(wamp_call_sup).

-behaviour(supervisor).

-export([start/0]).
-export([init/1]).


start() ->
    supervisor:start_link({local, wamp_call_sup}, ?MODULE, []).


init([]) ->
    {ok, Pools} = application:get_env(wamp_service, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                                  PoolArgs = [{name, {local, Name}},
                                              {worker_module, wamp_call_worker}] ++ SizeArgs,
                                  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.