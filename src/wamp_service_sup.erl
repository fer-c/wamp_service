%%%-------------------------------------------------------------------
%% @doc wamp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wamp_service_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-define(SUPERVISOR(Id, Mod, Args, Restart, Timeout), #{
    id => Id,
    start => {Mod, start_link, Args},
    restart => Restart,
    shutdown => Timeout,
    type => supervisor,
    modules => [Mod]
}).

-define(WORKER(Id, Mod, Args, Restart, Timeout), #{
    id => Id,
    start => {Mod, start_link, Args},
    restart => Restart,
    shutdown => Timeout,
    type => worker,
    modules => [Mod]
}).

-define(EVENT_MANAGER(Id, Restart, Timeout), #{
    id => Id,
    start => {gen_event, start_link, [{local, Id}]},
    restart => Restart,
    shutdown => Timeout,
    type => worker,
    modules => [dynamic]
}).


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
    Peers = wamp_service_config:get(peers),

    Children = maps:fold(
        fun(Name, Peer, Acc) ->
            Id = list_to_atom("wamp_service_peer_sup-" ++ atom_to_list(Name)),
            Sup = ?SUPERVISOR(
                Id,
                wamp_service_peer_sup,
                [Id, Name, Peer],
                permanent,
                5000
            ),
            [Sup | Acc]
        end,
        [],
        Peers
    ),

    Specs = {{one_for_one, 5, 60}, Children},
    {ok, Specs}.


%%====================================================================
%% Internal functions
%%====================================================================
