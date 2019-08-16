%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================
-module(wamp_service_service).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link(Opts) ->
    gen_server:start_link({local, wamp_caller} ,?MODULE, Opts, []).

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
    InitBackoff = proplists:get_value(backoff, Opts, 500),
    Backoff = backoff:init(InitBackoff, 120000),
    Reconnect = proplists:get_value(reconnect, Opts, false),
    State = #{host => Host, port => Port, realm => Realm,
              encoding => Encoding, retries => Retries, backoff => Backoff,
              reconnect => Reconnect},
    do_connect(State).

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
handle_call(Msg= {call, _, _, _, _}, From, State) ->
    _ = do_call(Msg, From, State),
    {noreply, State};
handle_call(Msg = {publish, _, _, _, _}, From, State) ->
    _ = do_publish(Msg, From, State),
    {noreply, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State = #{reconnect := Reconnect}) ->
    case Reconnect of
        true ->
            {ok, State1} = do_reconnect(State),
            {noreply, State1};
        false ->
            {stop, error, State}
    end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #{conn := Conn} ) ->
    awre:stop_client(Conn),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% =============================================================================
%% PRIVATE
%% =============================================================================

do_call({call, Uri, Args, ArgsKw, Details}, From, #{conn := Conn}) ->
    spawn(fun() ->
            ArgsKw1 = set_trace_id(ArgsKw),
            Details1 = wamp_service_utils:map_to_list(Details),
            Timeout = timeout(Details),
            try
                Res = awre:call(Conn, Details1, Uri, Args, ArgsKw1, Timeout),
                gen_server:reply(From, Res)
            catch
                Class:Reason:St ->
                    Error = handle_call_error(Class, Reason, St, Uri, Args, ArgsKw1, Details),
                    gen_server:reply(From, Error)
            end
          end).


do_publish({publish, Topic, Args, ArgsKw, Details}, From, #{conn := Conn}) ->
    spawn(fun() ->
            ArgsKw1 = set_trace_id(ArgsKw),
            Details1 = wamp_service_utils:map_to_list(Details),
            try
                Res = awre:publish(Conn, Details1, Topic, Args, ArgsKw1),
                gen_server:reply(From, Res)
            catch
                Class:Reason:St ->
                    Error = handle_call_error(Class, Reason, St, Topic, Args, ArgsKw1, Details),
                    gen_server:reply(From, Error)
            end
          end).


handle_call_error(Class, Reason, St, Uri, Args, ArgsKw, Details) ->
    _ = lager:error("handle call class=~p, reason=~p uri=~p  args=~p args_kw=~p details=~p stacktrace=~p",
                    [Class, Reason, Uri, Args, ArgsKw, Details, St]),
    case {Class, Reason} of
        {exit, {timeout, _}} ->
            {error, <<"com.magenta.error.timeout">>, Args, ArgsKw, Details};
        {error, #{code := _} = Error} ->
            Error;
        {_, _} ->
            {error, <<"com.magenta.error.internal">>, Args, ArgsKw, Details}
    end.


-spec trace_id(map()) -> binary() | undefined.
trace_id(Opts) ->
    maps:get(<<"trace_id">>, Opts, undefined).

-spec set_trace_id(map()) -> map().
set_trace_id(Opts) ->
    case trace_id(Opts) of
        undefined ->
            TraceId = wamp_service_trace_id:generate(),
            maps:put(<<"trace_id">>, TraceId, Opts);
        _ ->
            Opts
    end.

do_connect(State) ->
    #{host := Host, port := Port, realm := Realm, encoding := Encoding} = State,
    {ok, Conn} = awre:start_client(),
    try
        #{backoff := Backoff} = State,
        {ok, SessionId, RouterDetails} = awre:connect(Conn, Host, Port, Realm, Encoding),
        link(Conn),
        State1 = State#{conn => Conn, session_id => SessionId, details => RouterDetails,
                        attempts => 1, cbackoff => Backoff},
        {ok, State1}
    catch
        Class:Reason:St ->
            _ = lager:error("Connection error class=~p reason=~p stacktarce=~p",
                            [Class, Reason, St]),
            {error, Class}
    end.

do_reconnect(State) ->
    #{cbackoff := CBackoff, attempts := Attempts, retries := Retries} = State,
    case Attempts =< Retries of
        false ->
            _ = lager:error("Failed to reconnect :-("),
            exit(wamp_connection_error);
        true ->
            {Time, CBackoff1} = backoff:fail(CBackoff),
            case do_connect(State) of
                {ok, State1} ->
                    {ok, State1};
                {error, _} ->
                    _ = lager:info("Reconnecting, attempt ~p of ~p failed (retry in ~ps) ...",
                                    [Attempts, Retries, Time/1000]),
                    timer:sleep(Time),
                    do_reconnect(State#{attempts => Attempts + 1, cbackoff => CBackoff1})
            end
    end.

timeout(Details) ->
    maps:get(timeout, Details, 5000) + 100.
