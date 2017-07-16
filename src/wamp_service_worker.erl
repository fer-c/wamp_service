%%% Author      : Federico Repond
%%% Description :
%%% Created     : 29 Apr 2017 by Federico Repond
-module(wamp_service_worker).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Opts) ->
    process_flag(trap_exit, true),
    {ok, CP} =  re:compile(<<"^\\s+|\\s+$">>),
    erlang:put(trim_pattern, CP),
    %% connect to wamp broker
    Host = proplists:get_value(hostname, Opts),
    Port = proplists:get_value(port, Opts),
    Realm = proplists:get_value(realm, Opts),
    Encoding = proplists:get_value(encoding, Opts),
    Retries = proplists:get_value(retries, Opts, 10),
    Backoff = proplists:get_value(backoff, Opts, 100),
    {ok, Conn} = awre:start_client(),
    {ok, SessionId, _RouterDetails} = awre:connect(Conn, Host, Port, Realm, Encoding),
    link(Conn),
    %% and register procedures & subscribers
    Callbacks = register_callbacks(Conn, Opts),
    lager:info("done (~p).", [SessionId]),
    {ok, #{conn => Conn, session => SessionId, callbacks => Callbacks,  retries => Retries,
           backoff => Backoff, attempts => 0, opts => Opts}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) ->
%%                {reply, Reply, State} |
%%                {reply, Reply, State, Timeout} |
%%                {noreply, State} |
%%                {noreply, State, Timeout} |
%%                {stop, Reason, Reply, State} |
%%                {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({call, Uri, Args, Opts}, _From, #{conn := Conn} = State) ->
    Res = awre:call(Conn, [], Uri, Args, Opts),
    {reply, Res, State};
handle_call({publish, Topic, Msg, Opts}, _From, #{conn := Conn} = State) ->
    awre:publish(Conn, [], Topic, [Msg], Opts),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    lager:debug("request=~p state=~p", [Request, State]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({awre, {invocation, _, _, _, _, _} = Invocation},  State) ->
    lager:debug("Been called ~p ... will just handle it ...", [Invocation]),
    %% invocation of the rpc handler
    handle_invocation(Invocation, State),
    {noreply, State};
handle_info({awre, {event, _, _, _, _, _} = Publication}, State) ->
    lager:debug("Publication ~p ... will just handle it ...", [Publication]),
    %% invocation of the sub handler
    handle_event(Publication, State),
    {noreply, State};
handle_info(Msg, State = #{retries := Retries, backoff := Backoff, attempts := Attempts, opts := Opts}) ->
    lager:debug("msg=~p", [Msg]),
    lager:info("Reconnecting, attempt ~p of ~p (retry in ~ps) ...", [Attempts, Retries, Backoff/1000]),
    case Attempts of
        Retries ->
            throw(connection_error);
        _ ->
            try init(Opts) of % try to re-init
                {ok, NewState} ->
                    {noreply, NewState}
            catch
                _:_ ->
                    lager:info("Reconnection failed"),
                    timer:sleep(Backoff),
                    handle_info(retry, State#{backoff => backoff:increment(Backoff), attempts => Attempts + 1})
            end
    end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @private
handle_invocation({invocation, RequestId, RegistrationId, Details, Args, ArgsKw},
                  #{conn := Conn, callbacks := Callbacks}) ->
    try
        #{RegistrationId := #{handler := {Mod, Fun} = Handler, scopes := Scopes}} = Callbacks,
        lager:info("handle_cast invocation ~p ~p ~p ~p", [RegistrationId, Scopes, Handler, Args]),
        handle_security(ArgsKw, Scopes),
        Res = apply(Mod, Fun, args(Args) ++ [options(ArgsKw)]),
        handle_result(Conn, RequestId, Details, Res, ArgsKw),
        Res
    catch
        Class:Reason ->
            handle_error(Conn, RequestId, Class, Reason)
    end.

%% @private
handle_event({event, SubscriptionId, _PublicationId, _Details, Args, ArgsKw},
             #{callbacks := Callbacks}) ->
    try
        #{SubscriptionId := #{handler := {Mod, Fun} = Handler}} = Callbacks,
        lager:info("handle_cast event ~p ~p.", [SubscriptionId, Handler]),
        apply(Mod, Fun, args(Args) ++ [options(ArgsKw)])
    catch
        %% @TODO review error handling and URIs
        Error:Reason ->
            lager:error("Error: ~p:~p ~n ~p", [Error, Reason, erlang:get_stacktrace()])
    end.

%% @private
handle_result(Conn, RequestId, Details, Res, ArgsKw) ->
    case Res of
        undefined ->
            ok = awre:yield(Conn, RequestId, Details, [], ArgsKw);
        notfound ->
            throw(not_found);
        _ ->
            ok = awre:yield(Conn, RequestId, Details, [Res], ArgsKw)
    end.

%% @private
handle_error(Conn, RequestId, Class, Reason) ->
    lager:error("~s ~s", ["Unknown error",
                          lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
    case {Class, Reason} of
        %% @TODO review error handling and URIs
        {throw, unauthorized} ->
            Error = #{code => unauthorized, message => <<"Unauthorized user">>,
                      description => <<"The user does not have the required permissions to access the resource">>},
            awre:error(Conn, RequestId, Error, <<"com.magenta.error.unauthorized">>);
        {throw, not_found} ->
            Error = #{code => not_found, message => <<"Resource not found">>,
                      description => <<"The resourvce you are trying to retrieve does not exists">>},
            awre:error(Conn, RequestId, Error, <<"com.magenta.error.not_found">>);
        {error, #{code := _} = Error} ->
            awre:error(Conn, RequestId,  Error, <<"wamp.error.invalid_argument">>);
        {Class, Reason} ->
            Error = #{code => unknown_error, message => <<"Unknown error">>,
                      description => Reason},
            awre:error(Conn, RequestId, Error, <<"com.magenta.error.unknown_error">>)
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

%% @private
register_callbacks(Conn, Opts) ->
    Callbacks = proplists:get_value(callbacks, Opts),
    lists:foldl(fun ({procedure, Uri, MF, Scopes}, Acc) ->
                        lager:info("registering procedure ~p ... ", [Uri]),
                        {ok, RegistrationId} = awre:register(Conn, [{invoke, roundrobin}], Uri),
                        lager:info("registered (~p).", [RegistrationId]),
                        Acc#{RegistrationId => #{uri => Uri, handler => MF, scopes => Scopes}};
                    ({procedure, Uri, MF}, Acc) ->
                        lager:info("registering procedure ~p ... ", [Uri]),
                        {ok, RegistrationId} = awre:register(Conn, [{invoke, roundrobin}], Uri),
                        lager:info("registered (~p).", [RegistrationId]),
                        Acc#{RegistrationId => #{uri => Uri, handler => MF, scopes => []}};
                    ({subscription, Uri, MF}, Acc) ->
                        lager:info("registering subscription ~p ... ", [Uri]),
                        {ok, SubscriptionId} = awre:subscribe(Conn, [], Uri),
                        lager:info("registered (~p).", [SubscriptionId]),
                        Acc#{SubscriptionId => #{uri => Uri, handler => MF}}
                end, #{}, Callbacks).
