%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================


-module(wamp_call_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(Opts) ->
    process_flag(trap_exit, true),
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
    lager:info("worker pool done (~p).", [SessionId]),
    %% and register procedures & subscribershom
    {ok, #{conn => Conn, session => SessionId, retries => Retries, backoff => Backoff, attempts => 0, opts => Opts}}.

handle_call({call, Uri, Args, Opts}, _From, #{conn := Conn}=State) ->
    Res = awre:call(Conn, [], Uri, Args, Opts),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{conn := Conn}) ->
    ok = awre:stop_client(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
