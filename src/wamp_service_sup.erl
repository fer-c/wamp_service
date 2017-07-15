%%%-------------------------------------------------------------------
%% @doc wamp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wamp_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Pools} = application:get_env(wamp_service, session_pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                                PoolArgs = [{name, {local, Name}},
                                            {worker_module, wamp_service_worker}] ++ SizeArgs,
                                poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                        end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
