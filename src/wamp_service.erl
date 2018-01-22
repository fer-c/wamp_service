%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================

-module(wamp_service).

-export([call/3, call/4, maybe_error/1, publish/3, register/4, register/5, unregister/2]).


-spec call(Uri :: binary(), Args :: term(), Opts :: map()) -> {ok, any()} | {error, binary(), map()} | no_return().
call(Uri, Args, Opts) ->
    call(Uri, Args, Opts, 10000).

-spec call(Uri :: binary(), Args :: term(), Opts :: map(), Timeout :: pos_integer()) ->
                  {ok, any()} | {error, binary(), map()} | no_return().
call(Uri, Args, Opts, Timeout) when is_list(Args) ->
    process_flag(trap_exit, true),
    WampRes = poolboy:transaction(wamp_call_sessions,
                                  fun(Worker) ->
                                          gen_server:call(Worker, {call, Uri, Args, Opts, Timeout}, infinity)
                                  end, infinity),
    ok = lager:debug("call uri=~p result=~p", [Uri, WampRes]),
    case WampRes of
        {ok, _, [Res], _} ->
            {ok, Res};
        {ok, _, [], _} ->
            {ok, undefined};
        {error, _, Key, _, Map} ->
            {error, Key, Map}
    end.

-spec maybe_error(term()) -> {ok, any()} | no_return().
maybe_error(WampRes) ->
    case WampRes of
        Error = {error, _Key, _Map} ->
            error(Error);
        Res ->
            Res
    end.

-spec publish(Topic :: binary(), Args :: [any()], Opts :: map()) -> ok | no_return().
publish(Topic, Args, Opts) when is_list(Args) ->
    poolboy:transaction(wamp_call_sessions, fun(Worker) ->
                                                    gen_server:call(Worker, {publish, Topic, Args, Opts})
                                            end).

-spec register(atom(), procedure | subscription, binary(), {atom(), atom()} | function())
              -> ok | {error, binary()} | no_return().
register(Pool, procedure, Uri, Handler) ->
    register(Pool, procedure, Uri, Handler, []);
register(Pool, subscription, Uri, Handler) ->
    poolboy:transaction(Pool, fun(Worker) ->
                                      gen_server:call(Worker, {register, Uri, {subscription, Handler}})
                              end).

-spec register(atom(), procedure | subscription, binary(), {atom(), atom()} | function(), [binary()])
              -> ok | {error, binary()} | no_return().
register(Pool, procedure, Uri, Handler, Scopes) ->
    poolboy:transaction(Pool, fun(Worker) ->
                                      gen_server:call(Worker, {register, Uri, {procedure, Handler, Scopes}})
                              end).

-spec unregister(atom(), binary()) -> ok | {error, binary()} | no_return().
unregister(Pool, Uri) ->
    poolboy:transaction(Pool, fun(Worker) ->
                                      gen_server:call(Worker, {unregister, Uri})
                              end).
