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
    Opts = application:get_all_env(wamp_service),
    lager:info("Starting service..."),
    Procs = [{wamp_service_handler, {wamp_service_handler, start_link, [Opts]}, permanent, 5000, worker, []}],
    {ok, {{one_for_one, 1, 5}, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
