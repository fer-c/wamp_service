-module(wamp_ms_worker).
-behaviour(gen_server).

-record(state, {
        pool_type
        }).


%% API


%% GEN_SERVER CALLBACKS
-export([init/1]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).



%% =============================================================================
%% API
%% =============================================================================



%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================

init(_Opts) ->
    {ok, #state{pool_type = permanent}}.


handle_call({{Mod, Fun}, Args}, _From, State) ->
    %% @TODO add error handling
    Reply = apply(Mod, Fun, Args),
    {reply, Reply, State};
handle_call(Event, _From, State) ->
    lager:error("Unsupported call ~p", [Event]),
    {noreply, State}.


handle_cast({{Mod, Fun}, Args}, State) ->
    %% @TODO add error handling
    apply(Mod, Fun, Args),
    {noreply, State};
handle_cast(Event, State) ->
    lager:error("Unsupported cast ~p", [Event]),
    {noreply, State}.


handle_info(timeout, State) ->
    lager:info("Job timeout ~p", [State]),
    {noreply, State}.


terminate(normal, _State) ->
    ok;
terminate(shutdown, _State) ->
    ok;
terminate({shutdown, _}, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%% PRIVATE
%% =============================================================================


