%%%-------------------------------------------------------------------
%% @doc wamp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wamp_service_sup).

-behaviour(supervisor3).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor3:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, DispatcherSpec} = application:get_env(wamp_service, callee_dispatcher),
    {ok, ServiceSpec} = application:get_env(wamp_service, caller_service),
    {ok, {{one_for_one, 3, 60}, [
        {callee_dispatcher, {wamp_service_dispatcher, start_link, [DispatcherSpec]}, {permanent, 20}, 5000, worker, []},
        {caller_service, {wamp_service_service, start_link, [ServiceSpec]}, {permanent, 20}, 5000, worker, []}
    ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
