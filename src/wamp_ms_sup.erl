%%%-------------------------------------------------------------------
%% @doc wamp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wamp_ms_sup).

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
    Opts = [
            %% pool opts
            {pool_name, wamp_ms_worker_pool},
            {pool_capacity, 16 * erlang:system_info(schedulers) * 10000},
            {pool_size, 16 * erlang:system_info(schedulers)},
            %% wamp opts
            {hostname, "localhost"},
            {port, 8080},
            {realm, <<"realm1">>},
            {encoding,  msgpack},
            {services, [
                    {<<"com.example.add2">>, fun service:add/1},
                    {<<"com.leapsight.echo">>, fun service:echo/1}
                    ]}
            ],
    lager:info("Starting service..."),
    Procs = [{wamp_ms_handler,{wamp_ms_handler, start_link, [Opts]}, permanent, 5000, worker, []}],
    {ok, {{one_for_one, 10, 10}, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
