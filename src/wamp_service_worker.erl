-module(wamp_service_worker).
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


handle_call({{invocation, RequestId, RpcId, _Details, Args, ArgumentsKw}, #{con := Con, callbacks := Callbacks}}, _From, State) ->
    %% @TODO add error handling
    #{RpcId := #{handler := {Mod, Fun}}} = Callbacks,
    Res = apply(Mod, Fun, Args ++ [ArgumentsKw]),
    ok = awre:yield(Con, RequestId, [], [Res]),
    {reply, Res, State};
handle_call(Event, _From, State) ->
    lager:error("Unsupported call ~p", [Event]),
    {noreply, State}.


handle_cast({{invocation, RequestId, RpcId, _Details, Args, ArgumentsKw}, #{con := Con, callbacks := Callbacks}}, State) ->
    lager:info("handle invocation ~p ~p.", [RpcId, Callbacks]),
    #{RpcId := #{handler := {Mod, Fun}}} = Callbacks,
    try 
        Res = apply(Mod, Fun, Args ++ [ArgumentsKw]),
        ok = awre:yield(Con, RequestId, [], [Res]),
        {noreply, State}
    catch
        %% @TODO review error handling and URIs
        Error:Reason -> 
            lager:error("Error: ~p:~p", [Error, Reason]),
            awre:error(Con, RequestId, Error, Reason, <<"wamp.error.invalid_argument">>),
            {noreply, State}
    end;
handle_cast(Event, State) ->
    lager:error("Unsupported cast ~p", [Event]),
    {noreply, State}.


handle_info(timeout, State) ->
    lager:warning("Job timeout ~p", [State]),
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


