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

add(A, B, _Opts) ->
    lager:debug("add called, sent ~p + ~p.", [A, B]),
    A + B.

echo(Msg, _Opts) ->
    lager:debug("echo called, sent ~p.", [Msg]),
    Msg.

circular(Msg, Opts) ->
    wamp_service:call(<<"com.example.echo">>, [Msg], Opts).

circular_service_error(Opts) ->
    wamp_service:maybe_error(wamp_service:call(<<"com.example.service_error">>, [], Opts)).

unknown_error(_Opts) ->
    1 = 2.

notfound_error(_Opts) ->
    throw(not_found).

validation_error(_Opts) ->
    error(#{code => <<"invalid argument">>}).

service_error(_Opts) ->
    Error = #{code => service_error,
              message => <<"Service error">>,
              description => <<"Service Error">>},
    error(Error).

authorization_error(_Opts) ->
    Error = #{code => authorization_error,
              message => <<"Authorization error">>,
              description => <<"Authorization error">>},
    error(Error).

timeout(_Opts) ->
    timer:sleep(10000).

onhello(Msg, _Opts) ->
    lager:debug("event from com.example.onhello ~p.", [Msg]),
    ok.
