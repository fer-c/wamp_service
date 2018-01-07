%%% Author      : Federico Repond
%%% Description :
%%% Created     : 29 Apr 2017 by Federico Repond
-module(wamp_service_worker).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, handle_security/2]).

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
    Host = proplists:get_value(hostname, Opts),
    Port = proplists:get_value(port, Opts),
    Realm = proplists:get_value(realm, Opts),
    Encoding = proplists:get_value(encoding, Opts),
    Retries = proplists:get_value(retries, Opts, 10),
    Backoff = proplists:get_value(backoff, Opts, 100),
    CbConf = normalize_cb_conf(proplists:get_value(callbacks, Opts, #{})),
    State = #{
      host => Host,
      port => Port,
      realm => Realm,
      encoding => Encoding,
      retries => Retries,
      backoff => Backoff,
      attempts => 0,
      cb_conf => CbConf,
      callbacks => #{},
      inverted_ref => #{}
     },
    case connect(State) of
        {ok, NewState} ->
            {ok, NewState};
        error ->
            exit(wamp_connection_error)
    end.


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
handle_call({call, Uri, Args, Opts, Timeout}, _From, #{conn := Conn} = State) ->
    try
        Res = awre:call(Conn, [], Uri, Args, Opts, Timeout),
        {reply, Res, State}
    catch
        Class:Reason ->
            handle_call_error(Class, Reason, State)
    end;
handle_call({publish, Topic, Args, Opts}, _From, #{conn := Conn} = State) ->
    try
        awre:publish(Conn, [], Topic, Args, Opts),
        {reply, ok, State}
    catch
        Class:Reason ->
            handle_call_error(Class, Reason, State)
    end;
handle_call({add, Uri, Callback}, _From, State) ->
    %% invocation of the sub handler
    try
        State1 = add_callback(Uri, Callback, State),
        {reply, ok, State1}
    catch
        Class:_ ->
            {reply, {error, Class}, State}
    end;
handle_call({remove, Uri}, _From, State) ->
    %% invocation of the sub handler
    try
    State1 = remove_callback(Uri, State),
        {reply, ok, State1}
    catch
        Class:_ ->
            {reply, {error, Class}, State}
    end;
handle_call(Request, _From, State) ->
    ok = lager:debug("Unknown request=~p state=~p", [Request, State]),
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
    %% invocation of the rpc handler
    spawn(fun() -> handle_invocation(Invocation, State) end), %% TODO: handle load regulation?
    {noreply, State};
handle_info({awre, {event, _, _, _, _, _} = Publication}, State) ->
    %% invocation of the sub handler
    spawn(fun() -> handle_event(Publication, State) end), %% TODO: handle load regulation?
    {noreply, State};
handle_info({_Pid, {ok, #{<<"procedure">> := _}, _ , #{}}} = Msg, State) ->
    ok = lager:debug("Late message? msg=~p state=~p", [Msg, State]),
    {noreply, State};
handle_info(Msg, State = #{retries := Retries, backoff := Backoff, attempts := Attempts}) ->
    case Attempts =< Retries of
        false ->
            ok = lager:info("Failed to reconnect :-("),
            exit(wamp_connection_error);
        true ->
            ok = lager:debug("msg=~p state=~p", [Msg, State]),
            ok = lager:info("Reconnecting, attempt ~p of ~p (retry in ~ps) ...", [Attempts, Retries, Backoff/1000]),
            case connect(State) of % try to re-init
                {ok, NewState} ->
                    {noreply, NewState};
                error ->
                    ok = lager:info("Reconnection failed"),
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
    #{RegistrationId := #{handler := Handler, scopes := Scopes}} = Callbacks,
    try
        ok = lager:debug("handle_cast invocation request_id=~p registration_id=~p handler=~p",
                         [RequestId, RegistrationId, Handler]),
        ok = lager:debug("args=~p args_kw=~p, scope=~p", [Args, ArgsKw, Scopes]),
        handle_security(ArgsKw, Scopes),
        set_locale(ArgsKw),
        Res = exec_callback(Handler, args(Args) ++ [options(ArgsKw)]),
        handle_result(Conn, RequestId, Details, Res, ArgsKw)
    catch
        Class:Reason ->
            handle_invocation_error(Conn, RequestId, Handler, Class, Reason)
    end.

%% @private
handle_event({event, SubscriptionId, PublicationId, _Details, Args, ArgsKw},
             #{callbacks := Callbacks}) ->
    #{SubscriptionId := #{handler := Handler}} = Callbacks,
    try
        ok = lager:debug("handle_cast event subscription_id=~p publication_id=~p handler=~p",
                         [SubscriptionId, PublicationId, Handler]),
        ok = lager:debug("args=~p args_kw=~p", [Args, ArgsKw]),
        exec_callback(Handler, args(Args) ++ [options(ArgsKw)])
    catch
        %% @TODO review error handling and URIs
        Class:Reason ->
            lager:error("handle_event error: handler=~p, class=~p, reason=~p, stack=~p",
                        [Handler, Class, Reason, erlang:get_stacktrace()])
    end.

%% @private
handle_result(Conn, RequestId, Details, Res, ArgsKw) ->
    case Res of
        undefined ->
            ok = awre:yield(Conn, RequestId, Details, [], ArgsKw);
        notfound ->
            throw(not_found);
        {error, _, _} = Error ->
            throw(Error);
        _ ->
            ok = awre:yield(Conn, RequestId, Details, [Res], ArgsKw)
    end.

%% @private
handle_invocation_error(Conn, RequestId, Handler, Class, Reason) ->
    ok = lager:error("handle invocation error: handler=~p, class=~p, reason=~p, stack=~p",
                     [Handler, Class, Reason, erlang:get_stacktrace()]),
    case {Class, Reason} of
        %% @TODO review error handling and URIs
        {throw, unauthorized} ->
            Error = #{code => unauthorized, message => _(<<"Unauthorized user.">>),
                      description => _(<<"The user does not have the required permissions to access the resource.">>)},
            awre:error(Conn, RequestId, Error, <<"wamp.error.unauthorized">>);
        {throw, not_found} ->
            Error = #{code => not_found, message => _(<<"Resource not found.">>),
                      description => _(<<"The resource you are trying to retrieve does not exist.">>)},
            awre:error(Conn, RequestId, Error, <<"com.magenta.error.not_found">>);
        {_, {error, Key, Error}} ->
            awre:error(Conn, RequestId, Error, Key);
        {error, #{code := authorization_error} = Error} ->
            awre:error(Conn, RequestId, Error, <<"wamp.error.not_authorized">>);
        {error, #{code := service_error} = Error} ->
            awre:error(Conn, RequestId, Error, <<"com.magenta.error.internal_error">>);
        {error, #{code := _} = Error} ->
            awre:error(Conn, RequestId, Error, <<"wamp.error.invalid_argument">>);
        {Class, Reason} ->
            Error = #{code => unknown_error, message => _(<<"Unknown error.">>),
                      description => _(<<"There was an unknown error, please contact the administrator.">>)},
            awre:error(Conn, RequestId, Error, <<"com.magenta.error.unknown_error">>)
    end.

%% @private
handle_call_error(Class, Reason, State) ->
    ok = lager:error("handle call class=~p reason=~p, stack=~p", [Class, Reason, erlang:get_stacktrace()]),
    case {Class, Reason} of
        {exit, {timeout, _}} ->
            Details = #{code => timeout, message => _(<<"Service timeout.">>),
                        description => _(<<"There was a timeout resolving the operation.">>)},
            Error = {error, #{}, <<"com.magenta.error.timeout">>, #{}, Details},
            {reply, Error, State};
        {error, #{code := _} = Error} ->
            {reply, Error, State};
        {_, _} ->
            Details = #{code => unknown_error, message => _(<<"Unknown error.">>),
                        description => _(<<"There was an unknown error, please contact the administrator.">>)},
            Error = {error, #{}, <<"com.magenta.error.unknown_error">>, #{}, Details},
            {reply, Error, State}
    end.

%% @private
exec_callback({Mod, Fun}, Args) ->
    apply(Mod, Fun, Args);
exec_callback(Fun, Args) when is_function(Fun) ->
    apply(Fun, Args).

%% @private
handle_security(_, []) ->
    true;
handle_security(#{<<"security">> := #{<<"groups">> := Groups}}, ProcScope) ->
    Any = lists:any(fun(S) -> lists:member(S, ProcScope) end, Groups),
    Any orelse throw(unauthorized);
handle_security(_, _) ->
    throw(unauthorized).

%% @private
options(undefined) ->
    #{};
options(ArgsKw) when is_map(ArgsKw) ->
    ArgsKw.

%% @private
args(undefined) ->
    [];
args(Args) when is_list(Args) ->
    Args;
args(Arg) ->
    [Arg].

add_callback(Uri, Callback = {procedure, Fun, Scopes},
             State = #{cb_conf := CbConf, callbacks := Callbacks,
                       inverted_ref := InvertedRef, conn := Conn}) ->
    _ = lager:info("registering procedure uri=~p ... ", [Uri]),
    ok = validate_handler(Fun),
    {ok, RegistrationId} = awre:register(Conn, [{invoke, roundrobin}], Uri),
    _ = lager:info("registered reg_id=~p.", [RegistrationId]),
    State#{
      cb_conf => CbConf#{Uri => Callback},
      callbacks => Callbacks#{RegistrationId => #{uri => Uri, handler => Fun, scopes => Scopes}},
      inverted_ref => InvertedRef#{Uri => RegistrationId}
     };
add_callback(Uri, Callback = {subscription,  Fun},
             State = #{cb_conf := CbConf, callbacks := Callbacks,
                       inverted_ref := InvertedRef, conn := Conn}) ->
    _ = lager:info("registering subscription uri=~p ... ", [Uri]),
    ok = validate_handler(Fun),
    {ok, SubscriptionId} = awre:subscribe(Conn, [], Uri),
    _ = lager:info("registered subs_id=~p.", [SubscriptionId]),
    State#{
      cb_conf => CbConf#{Uri => Callback},
      callbacks => Callbacks#{SubscriptionId => #{uri => Uri, handler => Fun}},
      inverted_ref => InvertedRef#{Uri => SubscriptionId}
     }.

%% @private
remove_callback(Uri, State = #{inverted_ref := InvertedRef}) ->
    _ = lager:debug("deregistering procedure uri=~p ... ", [Uri]),
    case maps:get(Uri, InvertedRef, undefined) of
        undefined ->
            _ = lager:warning("Attemping to remove invalid registration uri=~p", [Uri]),
            State;
        _ ->
            #{inverted_ref := InvertedRef = #{Uri := Id}, callbacks := Callbacks,
              cb_conf := CbConf = #{Uri := Conf}, conn := Conn} = State,
            do_remove_callback(Conn, Id, Conf),
            State#{inverted_ref => maps:remove(Uri, InvertedRef),
                   callbacks => maps:remove(Id, Callbacks),
                   cb_conf => maps:remove(Uri, CbConf)}
    end.

do_remove_callback(Conn, Id, {procedure, _, _}) ->
    _ = lager:debug("deregistering procedure id=~p ... ", [Id]),
    awre:unregister(Conn, Id);
do_remove_callback(Conn, Id, {subscription, _}) ->
    _ = lager:debug("deregistering subscription id=~p ... ", [Id]),
    awre:unsubscribe(Conn, Id).

%% @private
register_callbacks(State = #{cb_conf := CbConf}) ->
    maps:fold(fun (Uri, Cb, St) ->
                      add_callback(Uri, Cb, St)
              end, State#{cb_conf => #{}, callbacks => #{} ,inverted_ref => #{}}, CbConf).


%% @ private
connect(State = #{host := Host, port := Port, realm := Realm, encoding := Encoding}) ->
    try
        {ok, Conn} = awre:start_client(),
        {ok, SessionId, _RouterDetails} = awre:connect(Conn, Host, Port, Realm, Encoding),
        link(Conn),
        %% and register procedures & subscribers
        State1 = State#{conn => Conn, session => SessionId},
        State2 = register_callbacks(State1),
        _ = lager:info("Session started session_id=~p", [SessionId]),
        {ok, State2}
    catch
        Class:Reason ->
            _ = lager:error("Connection error class=~p reason=~p stacktarce=~p",
                            [Class, Reason, erlang:get_stacktrace()]),
            error
    end.

%% @private
set_locale(ArgsKw) ->
    Sec = maps:get(<<"security">>, options(ArgsKw), #{}),
    Locale = maps:get(<<"locale">>, Sec, <<"es_AR">>),
    erlang:put(locale, normalize_locale(Locale)).

normalize_locale(Locale) ->
    re:replace(Locale, <<"-">>, <<"_">>,  [{return, binary}]).

validate_handler(Fun) when is_function(Fun) ->
    ok;
validate_handler(Handler = {M, F}) ->
    Exports = M:module_info(exports),
    case lists:keyfind(F, 1, Exports) of
        false ->
            lager:error("Invalid handler ~p", [Handler]),
            exit(invalid_handler, "The handler you're trying to register does not exist.");
        _ ->
            ok
    end;
validate_handler(Handler) ->
    lager:error("Invalid handler ~p", [Handler]),
    exit(invalid_handler, <<"The handler you're trying to register is invalid",
                            "(should be either Fun | {Mod, FunName}).">>).

normalize_cb_conf(CbConf = #{}) ->
    CbConf;
normalize_cb_conf(CbConf) when is_list(CbConf) ->
    lists:foldl(fun
                    ({subscription, Uri, MF}, Acc)  ->
                       Acc#{Uri => {subscription, MF}};
                    ({procedure, Uri, MF}, Acc)  ->
                       Acc#{Uri => {procedure, MF, []}};
                    ({procedure, Uri, MF, Scopes}, Acc) ->
                       Acc#{Uri => {procedure, MF, Scopes}}
               end,
                #{}, CbConf).
