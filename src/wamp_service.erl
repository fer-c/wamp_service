%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================

-module(wamp_service).

-export([call/3, publish/3]).


-spec call(Uri :: binary(), Args :: term(), Opts :: map()) -> term() | no_return().
call(Uri, Args, Opts) ->
    WampRes = poolboy:transaction(wamp_sessions, fun(Worker) ->
                                                         gen_server:call(Worker, {call, Uri, Args, Opts})
                                                 end),
    lager:debug("Call Result: ~p", [WampRes]),
    case WampRes of
        {ok, _, [Res], _} ->
            Res;
        {ok, _, [], _} ->
            ok;
        Error ->
            throw(Error)
    end.

-spec publish(Topic :: binary(), Msg :: term(), Opts :: map()) -> ok | no_return().
publish(Topic, Msg, Opts) ->
    poolboy:transaction(wamp_sessions, fun(Worker) ->
                                               gen_server:call(Worker, {publish, Topic, Msg, Opts})
                                       end).
