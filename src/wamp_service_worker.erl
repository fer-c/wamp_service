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


handle_call({{invocation, _, _, _, _, _}, _} = Invocation, From, State) ->
    handle_invocation(Invocation, From, State);
handle_call({{event, _, _, _, _, _}, _} = Event, From, State) ->
    handle_event(Event, From, State);
handle_call(Event, _From, State) ->
    lager:error("Unsupported call ~p", [Event]),
    {noreply, State}.


handle_cast({{invocation, _, _, _, _, _}, _} = Invocation, State) ->
    handle_invocation(Invocation, undefined, State);
handle_cast({{event, _, _, _, _, _}, _} = Event, State) ->
    handle_event(Event, undefined, State);
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

handle_invocation({{invocation, RequestId, RegistrationId, Details, Args, ArgsKw},
                   #{con := Con, callbacks := Callbacks}}, _From, State) ->
    try
        #{RegistrationId := #{handler := {Mod, Fun} = Handler, scopes := Scopes}} = Callbacks,
        lager:info("handle_cast invocation ~p ~p ~p.", [RegistrationId, Handler, Scopes]),
        handle_security(ArgsKw, Scopes),
        Res = apply(Mod, Fun, Args ++ [ArgsKw]),
        handle_result(Con, RequestId, Details, Res, ArgsKw),
        {noreply, State}
    catch
        %% @TODO review error handling and URIs
        unauthorized ->
            awre:error(Con, RequestId, <<"unauthorized">>,  <<"Unauthorized user">>, <<"com.magenta.error.unauthorized">>);
        not_found ->
            awre:error(Con, RequestId, <<"not_found">>, <<"Resource not found">>, <<"com.magenta.error.not_found">>);
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()]),
            awre:error(Con, RequestId, Error, Reason, <<"wamp.error.invalid_argument">>),
            {noreply, State}
    end.


handle_event({{event, SubscriptionId, _PublicationId, _Details, Args, ArgsKw},
              #{callbacks := Callbacks}}, _From, State) ->
    try
        #{SubscriptionId := #{handler := {Mod, Fun} = Handler}} = Callbacks,
        lager:info("handle_cast event ~p ~p.", [SubscriptionId, Handler]),
        apply(Mod, Fun, Args ++ [ArgsKw]),
        {noreply, State}
    catch
        %% @TODO review error handling and URIs
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.


handle_result(Con, RequestId, Details, Res, ArgsKw) ->
    case Res of
        undefined ->
            ok = awre:yield(Con, RequestId, Details, [], ArgsKw);
        notfound ->
            throw(not_found);
        _ ->
            ok = awre:yield(Con, RequestId, Details, [Res], ArgsKw)
    end.


handle_security(_ArgsKw, []) ->
    ok;
handle_security(_ArgsKw, _Scopes) ->
    throw({unauthorized}).
