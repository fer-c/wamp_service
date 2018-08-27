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
    {ok, DispatcherSpec} = application:get_env(wamp_service, callee_dispatcher),
    {ok, ServiceSpec} = application:get_env(wamp_service, caller_service),
    {workers, DWorkers} = lists:keyfind(workers, 1, DispatcherSpec),
    {workers, SWorkers} = lists:keyfind(workers, 1, ServiceSpec),
    ArgsD = [callee_dispatcher, [
                {workers, DWorkers}, {worker, {wamp_service_dispatcher, DispatcherSpec}},
                {strategy, {one_for_all, 0, 1}}]],
    ArgsS = [caller_service, [
                {workers, SWorkers}, {worker, {wamp_service_service, ServiceSpec}},
                {strategy, {one_for_all, 0, 1}}]],
    {ok, {{one_for_one, 5, 10}, [
        {callee_dispatcher, {wpool, start_sup_pool, ArgsD}, permanent, 5000, supervisor, []},
        {caller_service, {wpool, start_sup_pool, ArgsS}, permanent, 5000, supervisor, []}
    ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
