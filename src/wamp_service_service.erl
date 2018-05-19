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
    gen_server:start_link(?MODULE, Opts, []).

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
    Start = proplists:get_value(backoff_start, Opts, 1000),
    Max = proplists:get_value(backoff_max, Opts, 1000 * 60 * 2),
    State = #{
      host => Host,
      port => Port,
      realm => Realm,
      encoding => Encoding,
      retries => Retries,
      backoff => backoff:init(Start, Max)
     },
    case wamp_service_utils:connect(Host, Port, Realm, Encoding) of
        {ok, {Conn, SessionId}} ->
            State1 = State#{conn => Conn, session => SessionId},
            {ok, State1};
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
    Opts1 = set_trace_id(Opts),
    try
        Res = awre:call(Conn, [], Uri, Args, Opts1, Timeout),
        {reply, Res, State}
    catch
        Class:Reason ->
            handle_call_error(Class, Reason, Uri, Args, Opts1, State)
    end;
handle_call({publish, Topic, Args, ArgsKw}, _From, #{conn := Conn} = State) ->
    ArgsKw1 = set_trace_id(ArgsKw),
    try
        awre:publish(Conn, [], Topic, Args, ArgsKw1),
        {reply, ok, State}
    catch
        Class:Reason ->
            handle_call_error(Class, Reason, Topic, Args, ArgsKw1, State)
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({call, From, Uri, Args, Opts, Timeout}, #{conn := Conn} = State) ->
    Opts1 = set_trace_id(Opts),
    try
        awre:async_call(Conn, From, [], Uri, Args, Opts1, Timeout),
        {noreply, State}
    catch
        Class:Reason ->
            handle_call_error(Class, Reason, Uri, Args, Opts1, State)
    end;
handle_cast(_, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    #{host := Host, port:= Port, realm := Realm,
      encoding := Encoding, retries := Retries, backoff := Backoff} = State,
    {ok, {Conn, SessionId}} = wamp_service_utils:reconnect(Host, Port, Realm, Encoding, Backoff, Retries, 0),
    State1 = State#{conn => Conn, session => SessionId},
    {noreply, State1}.

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
handle_call_error(Class, Reason, Uri, Args, ArgsKw, State) ->
    _ = lager:error("handle call class=~p, reason=~p, uri=~p,  args=~p, args_kw=~p, stacktrace=~p",
                    [Class, Reason, Uri, Args, ArgsKw, erlang:get_stacktrace()]),
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


-spec trace_id(map()) -> binary().
trace_id(Opts) ->
    maps:get(<<"trace_id">>, Opts, undefined).

-spec set_trace_id(map()) -> map().
set_trace_id(Opts) ->
    case trace_id(Opts) of
        undefined ->
            TraceId = <<>>,
            maps:put(<<"trace_id">>, TraceId, Opts);
        _ ->
            Opts
    end.
