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


handle_call({{invocation, RequestId, RegistrationId, _Details, Args, ArgumentsKw}, #{con := Con, callbacks := Callbacks}}, _From, State) ->
    try
        #{RegistrationId := #{handler := {Mod, Fun} = Handler}} = Callbacks,
        lager:info("handle_cast invocation ~p ~p.", [RegistrationId, Handler]),
        Res = apply(Mod, Fun, Args ++ [ArgumentsKw]),
        handle_result(Con, RequestId, Res),
        {reply, Res, State}
    catch
        %% @TODO review error handling and URIs
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()]),
            awre:error(Con, RequestId, Error, Reason, <<"wamp.error.invalid_argument">>),
            {noreply, State}
    end;
handle_call({{event, SubscriptionId, _PublicationId, _Details, Args, ArgumentsKw}, #{callbacks := Callbacks}}, _From, State) ->
    try
        #{SubscriptionId := #{handler := {Mod, Fun} = Handler}} = Callbacks,
        lager:info("handle_cast event ~p ~p.", [SubscriptionId, Handler]),
        apply(Mod, Fun, Args ++ [ArgumentsKw]),
        {noreply, State}
    catch
        %% @TODO review error handling and URIs
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end;
handle_call(Event, _From, State) ->
    lager:error("Unsupported call ~p", [Event]),
    {noreply, State}.


handle_cast({{invocation, RequestId, RegistrationId, _Details, Args, ArgumentsKw}, #{con := Con, callbacks := Callbacks}}, State) ->
    try
        #{RegistrationId := #{handler := {Mod, Fun} = Handler}} = Callbacks,
        lager:info("handle_cast invocation ~p ~p.", [RegistrationId, Handler]),
        Res = apply(Mod, Fun, Args ++ [ArgumentsKw]),
        handle_result(Con, RequestId, Res),
        {noreply, State}
    catch
        %% @TODO review error handling and URIs
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()]),
            awre:error(Con, RequestId, Error, Reason, <<"wamp.error.invalid_argument">>),
            {noreply, State}
    end;
handle_cast({{event, SubscriptionId, _PublicationId, _Details, Args, ArgumentsKw}, #{callbacks := Callbacks}}, State) ->
    try
        #{SubscriptionId := #{handler := {Mod, Fun} = Handler}} = Callbacks,
        lager:info("handle_cast event ~p ~p.", [SubscriptionId, Handler]),
        apply(Mod, Fun, Args ++ [ArgumentsKw]),
        {noreply, State}
    catch
        %% @TODO review error handling and URIs
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()]),
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

handle_result(Con, RequestId, Res) ->
    case Res of
        undefined ->
            ok = awre:yield(Con, RequestId, [], []);
        notfound ->
            awre:error(Con, RequestId, <<"not_found">>, <<"Resource not found">>, <<"com.magenta.error.not_found">>);
        _ ->
            ok = awre:yield(Con, RequestId, [], [Res])
    end.
