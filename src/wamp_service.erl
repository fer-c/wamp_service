%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================

-module(wamp_service).

-export([call/3, call/4, publish/3]).


-spec call(Uri :: binary(), Args :: term(), Opts :: map()) -> term() | no_return().
call(Uri, Args, Opts) ->
    call(Uri, Args, Opts, 10000).

-spec call(Uri :: binary(), Args :: term(), Opts :: map(), Timeout :: pos_integer()) -> term() | no_return().
call(Uri, Args, Opts, Timeout) ->
    process_flag(trap_exit, true),
    WampRes = poolboy:transaction(wamp_servicec_sessions, fun(Worker) ->
                                                         gen_server:call(Worker, {call, Uri, Args, Opts, Timeout}, infinity)
                                                 end, infinity),
    lager:debug("call uri=~p result=~p", [Uri, WampRes]),
    case WampRes of
        {ok, _, [Res], _} ->
            Res;
        {ok, _, [], _} ->
            ok;
        {error,_,Key, _, Map} ->
            {error, Key, Map};
        Other ->
            Other
    end.

-spec publish(Topic :: binary(), Msg :: term(), Opts :: map()) -> ok | no_return().
publish(Topic, Msg, Opts) ->
    poolboy:transaction(wamp_servicec_sessions, fun(Worker) ->
                                               gen_server:call(Worker, {publish, Topic, Msg, Opts})
                                       end).
