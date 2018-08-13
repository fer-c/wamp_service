%% Demo service
-module(wamp_service_example).

-export([add/3]).
-export([echo/2]).
-export([circular/2]).
-export([circular_service_error/1]).
-export([unknown_error/1]).
-export([notfound_error/1]).
-export([validation_error/1]).
-export([service_error/1]).
-export([authorization_error/1]).
-export([timeout/1]).
-export([onhello/2]).
-export([onadd/3]).

-spec add(number(), number(), map()) -> number().
add(A, B, _Opts) ->
    A + B.

-spec echo(any(), map()) -> any().
echo(Msg, _Opts) ->
    Msg.

-spec circular(any(), map()) -> {ok, any()} | {error, binary(), map()} | no_return().
circular(Msg, Opts) ->
    {ok, Res} = wamp_service:call(<<"com.example.echo">>, [Msg], Opts),
    Res.

-spec circular_service_error(map()) -> {ok, any()} | no_return().
circular_service_error(Opts) ->
    wamp_service:maybe_error(wamp_service:call(<<"com.example.service_error">>, [], Opts)).

-spec unknown_error(map()) -> no_return().
unknown_error(_Opts) ->
    error("no match of right hand side value 2").

-spec notfound_error(map()) -> no_return().
notfound_error(_Opts) ->
    throw(not_found).

-spec validation_error(map()) -> no_return().
validation_error(_Opts) ->
    error(#{code => <<"invalid argument">>}).

-spec service_error(map()) -> no_return().
service_error(_Opts) ->
    Error = #{code => service_error,
              message => <<"Service error">>,
              description => <<"Service Error">>},
    error(Error).

-spec authorization_error(map()) -> no_return().
authorization_error(_Opts) ->
    Error = #{code => authorization_error,
              message => <<"Authorization error">>,
              description => <<"Authorization error">>},
    error(Error).

-spec timeout(map()) -> ok.
timeout(_Opts) ->
    timer:sleep(15000).

-spec onhello(any(),  map()) -> ok.
onhello(Msg, _Opts) ->
    ok = lager:debug("event from com.example.onhello ~p.", [Msg]),
    ok.

onadd(A, B, _Opts) ->
    ok = lager:debug("event from com.example.onadd ~p.", [A + B]),
    ok.
