%% Demo service
-module(wamp_service_example).

-export([add/4]).
-export([echo/3]).
-export([multiple_results/2]).
-export([circular/3]).
-export([circular_service_error/2]).
-export([unknown_error/2]).
-export([notfound_error/2]).
-export([validation_error/2]).
-export([service_error/2]).
-export([authorization_error/2]).
-export([timeout/2]).
-export([onhello/2]).
-export([onadd/3]).

-spec add(number(), number(), map(), map()) -> number().
add(A, B, _Opts, _Details) ->
    ok = lager:debug("add called, sent ~p + ~p.", [A, B]),
    A + B.

-spec echo(any(), map(), map()) -> any().
echo(Msg, _Opts, _Details) ->
    ok = lager:debug("echo called..."),
    timer:sleep(2500),
    ok = lager:debug("echo sent ~p.", [Msg]),
    Msg.

-spec multiple_results(map(), map()) -> list().
multiple_results(_Opts, _Details) ->
    [1, 2, 3].

-spec circular(any(), map(), map()) -> {ok, list(), map(), map()} | {error, binary(), map(), map()} | no_return().
circular(Msg, Opts, Details) ->
    Res = {ok, _, _, _} = wamp_service:call(<<"com.example.echo">>, [Msg], Opts, Details),
    Res.

-spec circular_service_error(map(), map()) -> {ok, list(), map(), map()} | no_return().
circular_service_error(Opts, Details) ->
    wamp_service:maybe_error(wamp_service:call(<<"com.example.service_error">>, [], Opts, Details)).


-spec unknown_error(map(), map()) -> no_return().
unknown_error(_Opts, _Details) ->
    error("no match of right hand side value 2").

-spec notfound_error(map(), map()) -> no_return().
notfound_error(_Opts, _Details) ->
    throw(not_found).

-spec validation_error(map(), map()) -> no_return().
validation_error(_Opts, _Details) ->
    error(#{code => <<"invalid argument">>}).

-spec service_error(map(), map()) -> no_return().
service_error(_Opts, _Details) ->
    Error = #{code => service_error,
              message => <<"Service error">>,
              description => <<"Service Error">>},
    error(Error).

-spec authorization_error(map(), map()) -> no_return().
authorization_error(_Opts, _Details) ->
    Error = #{code => authorization_error,
              message => <<"Authorization error">>,
              description => <<"Authorization error">>},
    error(Error).

-spec timeout(map(), map()) -> ok.
timeout(_Opts, _Details) ->
    timer:sleep(15000).

-spec onhello(any(),  map()) -> ok.
onhello(Msg, _Opts) ->
    ok = lager:debug("event from com.example.onhello ~p.", [Msg]),
    ok.

onadd(A, B, _Opts) ->
    ok = lager:debug("event from com.example.onadd ~p.", [A + B]),
    ok.
