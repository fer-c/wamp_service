%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================

-module(wamp_service).

-define(DELTA, 100).

-export([call/3, call/4, maybe_error/1, publish/3, register/3, register/4, unregister/1, status/0]).


-spec call(Uri :: binary(), Args :: term(), ArgsKw :: map()) -> {ok, any()} | {error, binary(), map()} | no_return().
call(Uri, Args, ArgsKw) ->
    call(Uri, Args, ArgsKw, 10000).

-spec call(Uri :: binary(), Args :: term(), ArgsKw :: map(), Timeout :: pos_integer()) ->
                  {ok, any()} | {error, binary(), map()} | no_return().
call(Uri, Args, ArgsKw, Timeout)
        when is_list(Args) andalso is_map(ArgsKw) andalso is_integer(Timeout) ->
    try
        WampRes = gen_server:call(wamp_caller, {call, Uri, Args, ArgsKw, Timeout}, Timeout + 100),
        case WampRes of
            {ok, _, [Res], _} ->
                _ = lager:debug("call uri=~p result=~p", [Uri, Res]),
                {ok, Res};
            {ok, _, [], _} ->
                _ = lager:debug("call uri=~p result=~p", [Uri, undefined]),
                {ok, undefined};
            {ok, _, Res = [_, _ | _], _} ->
                _ = lager:debug("call uri=~p result=~p", [Uri, Res]),
                {ok, Res};
            {error, _, Key, _, Map} ->
                _ = lager:debug("call uri=~p key=~p error=~p", [Uri, Key, Map]),
                {error, Key, Map}
        end
    catch
        _:{timeout, _} ->
            {error, <<"com.magenta.error.timeout">>, #{code => timeout, description => Uri}}
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
    gen_server:call(wamp_caller, {publish, Topic, Args, Opts}).

-spec register(procedure | subscription, binary(), {atom(), atom()} | function())
              -> ok | {error, binary()} | no_return().
register(procedure, Uri, Handler) ->
    register(procedure, Uri, Handler, []);
register(subscription, Uri, Handler) ->
    gen_server:cast(wamp_dispatcher, {register, Uri, {subscription, Handler}}).

-spec register(procedure | subscription, binary(), {atom(), atom()} | function(), [binary()])
              -> ok | {error, binary()} | no_return().
register(procedure, Uri, Handler, Scopes) ->
    wamp_service:unregister(Uri),
    gen_server:cast(wamp_dispatcher, {register, Uri, {procedure, Handler, Scopes}}).

-spec unregister(binary()) -> ok | {error, binary()} | no_return().
unregister(Uri) ->
    gen_server:cast(wamp_dispatcher, {unregister, Uri}).

-spec status() -> map().
status() ->
    gen_server:call(wamp_dispatcher, status).
