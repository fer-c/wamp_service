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
-export([handle_security/2]).



%% =============================================================================
%% API
%% =============================================================================



%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================

init(_Opts) ->
    {ok, CP} =  re:compile(<<"^\\s+|\\s+$">>),
    erlang:put(trim_pattern, CP),
    {ok, #state{pool_type = permanent}}.


handle_call({{invocation, _, _, _, _, _}, _} = Invocation, From, State) ->
    Res = handle_invocation(Invocation, From, State),
    {reply, Res, State};
handle_call({{event, _, _, _, _, _}, _} = Event, From, State) ->
    handle_event(Event, From, State),
    {noreply, State};
handle_call(Event, _From, State) ->
    lager:error("Unsupported call ~p", [Event]),
    {noreply, State}.


handle_cast({{invocation, _, _, _, _, _}, _} = Invocation, State) ->
    handle_invocation(Invocation, undefined, State),
    {noreply, State};
handle_cast({{event, _, _, _, _, _}, _} = Event, State) ->
    handle_event(Event, undefined, State),
    {noreply, State};
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
                   #{con := Con, callbacks := Callbacks}}, _From, _State) ->
    try
        #{RegistrationId := #{handler := {Mod, Fun} = Handler, scopes := Scopes}} = Callbacks,
        lager:info("handle_cast invocation ~p ~p ~p ~p", [RegistrationId, Scopes, Handler, Args]),
        handle_security(ArgsKw, Scopes),
        Res = apply(Mod, Fun, args(Args) ++ [options(ArgsKw)]),
        handle_result(Con, RequestId, Details, Res, ArgsKw),
        Res
    catch
        %% @TODO review error handling and URIs
        throw:unauthorized ->
            lager:error("Unauthorized error"),
            lager:debug("~p", [erlang:get_stacktrace()]),
            awre:error(Con, RequestId, unauthorized, "Unauthorized user", <<"com.magenta.error.unauthorized">>);
        throw:not_found ->
            lager:error("Not found error"),
            lager:debug("~p", [erlang:get_stacktrace()]),
            awre:error(Con, RequestId, not_found, "Resource not found", <<"com.magenta.error.not_found">>);
        error:#{code := code, message := _Message, description := Description} ->
            lager:error("Validation error"),
            lager:debug("~p", [erlang:get_stacktrace()]),
            awre:error(Con, RequestId, invalid_argument, binary_to_list(Description), <<"wamp.error.invalid_argument">>);
        _:Reason ->
            lager:error("Unknown error"),
            lager:debug("reason=~p stacktrace=~p", [Reason, erlang:get_stacktrace()]),
            awre:error(Con, RequestId, unknown_error, Reason, <<"com.magenta.error.unknown_error">>)
    end.


handle_event({{event, SubscriptionId, _PublicationId, _Details, Args, ArgsKw},
              #{callbacks := Callbacks}}, _From, _State) ->
    try
        #{SubscriptionId := #{handler := {Mod, Fun} = Handler}} = Callbacks,
        lager:info("handle_cast event ~p ~p.", [SubscriptionId, Handler]),
        apply(Mod, Fun, args(Args) ++ [options(ArgsKw)])
    catch
        %% @TODO review error handling and URIs
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()])
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


%% @private
handle_security(_ArgsKw, []) ->
    true;
handle_security(#{<<"security">> := #{<<"scope">> := ScopeBin}}, ProcScope) ->
    Scope = binary:split(ScopeBin, <<",">>, [trim_all, global]),
    Any = lists:any(fun(S) -> lists:member(trim(S), ProcScope) end, Scope),
    Any orelse throw(unauthorized);
handle_security(_, _) ->
    throw(unauthorized).

%% @private
trim(Bin) ->
    re:replace(Bin, erlang:get(trim_pattern), "", [{return, binary}, global]).

%% @private
options(undefined) ->
    #{};
options(ArgsKw) when is_map(ArgsKw) ->
    ArgsKw.

%% @private
args(undefined) ->
    [];
args(Args) when is_list(Args) ->
    Args.
