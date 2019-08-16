%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================

-module(wamp_service).

-define(DELTA, 100).

-export([call/3, call/4, publish/3, publish/4, register/3, register/4, unregister/1, status/0]).


-spec call(Uri :: binary(), Args :: term(), ArgsKw :: map()) ->
            {ok, Args :: list(), ArgsKw :: map(), Details :: map()} |
            {error, URI :: binary(), Args :: list(), ArgsKw:: map(), Details :: map()} |
            no_return().
call(Uri, Args, ArgsKw) ->
    call(Uri, Args, ArgsKw, #{}).

-spec call(Uri :: binary(), Args :: term(), ArgsKw :: map(), Details :: map()) ->
            {ok, Args :: list(), ArgsKw :: map(), Details :: map()} |
            {error, URI :: binary(), Args :: list(), ArgsKw :: map(), Details :: map()} |
            no_return().
call(Uri, Args, ArgsKw, Details)
        when is_list(Args) andalso is_map(ArgsKw) andalso is_map(Details) ->
    try
        Timeout = timeout(Details),
        WampRes = gen_server:call(wamp_caller, {call, Uri, Args, ArgsKw, Details}, Timeout),
        case WampRes of
            {ok, RDetails, RArgs, RArgsKw} ->
                _ = lager:debug("call uri=~p args=~p args_kw=~p details=~p", [Uri, RArgs, RArgsKw, RDetails]),
                {ok,  RArgs, RArgsKw, RDetails};
            {error, RDetails, RUri, RArgs, RArgsKw} ->
                _ = lager:debug("call uri=~p args=~p args_kw=~p details=~p", [RUri, RArgs, RArgsKw, RDetails]),
                {error, RUri, RArgs, RArgsKw, RDetails}
        end
    catch
        _:{timeout, _} ->
            {error, <<"com.magenta.error.timeout">>, Args, ArgsKw, Details}
    end.

-spec publish(Topic :: binary(), Args :: [any()], ArgsKw :: map()) ->
            ok |
            {error, URI :: binary(), Args :: list(), ArgsKw :: map(), Details :: map()} |
            no_return().
publish(Topic, Args, ArgsKw) ->
    publish(Topic, Args, ArgsKw, #{}).

-spec publish(Topic :: binary(), Args :: [any()], ArgsKw :: map(), Details :: map()) ->
            ok |
            {error, URI :: binary(), Args :: list(), ArgsKw :: map(), Details :: map()} |
            no_return().
publish(Topic, Args, ArgsKw, Details) when is_list(Args) ->
    ok = gen_server:call(wamp_caller, {publish, Topic, Args, ArgsKw, Details}).

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


timeout(Details) ->
    maps:get(timeout, Details, 5000) + 100.
