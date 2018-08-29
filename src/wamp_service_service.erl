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
    Host = proplists:get_value(hostname, Opts),
    Port = proplists:get_value(port, Opts),
    Realm = proplists:get_value(realm, Opts),
    Encoding = proplists:get_value(encoding, Opts),
        {ok, Conn} = awre:start_client(),
    {ok, SessionId, _RouterDetails} = awre:connect(Conn, Host, Port, Realm, Encoding),
    link(Conn),
    State1 = #{conn => Conn, session => SessionId},
    {ok, State1}.

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
handle_call(Msg = {publish, _, _, _}, _From, State) ->
    ok = do_publish(Msg, State),
    {reply, ok, State}.
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
handle_cast(Msg = {publish, _, _, _}, State) ->
    ok = do_publish(Msg, State),
    {noreply, State}.


handle_info(_Msg, State) ->
    {stop, State}.

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
do_publish({publish, Topic, Args, ArgsKw}, #{conn := Conn} = State) ->
    ArgsKw1 = set_trace_id(ArgsKw),
    awre:publish(Conn, [], Topic, Args, ArgsKw1).

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
            Details = #{code => internal_error, message => _(<<"Internal error.">>),
                        description => _(<<"There was an internal error, please contact the administrator.">>)},
            Error = {error, #{}, <<"com.magenta.error.internal">>, #{}, Details},
            {reply, Error, State}
    end.


-spec trace_id(map()) -> binary().
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
