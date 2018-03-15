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
    {ok, DispatcherSpec} = application:get_env(wamp_service, dispatcher),
    {ok, ServiceSpec} = application:get_env(wamp_service, service),
    Pools = [{dispatcher, wamp_service_dispatcher, DispatcherSpec}, {service, wamp_service_service, ServiceSpec}],
    PoolSpecs = lists:map(fun({Name, Worker, {SizeArgs, WorkerArgs}}) ->
                                  PoolArgs = [{name, {local, Name}},
                                              {worker_module, Worker}] ++ SizeArgs,
                                  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
