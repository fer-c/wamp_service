%% Demo service
-module(wamp_service_example).

-export([add/3]).
-export([echo/2]).
-export([circular/2]).
-export([error/1]).
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

error(_Opts) ->
	1 = 2.

timeout(_Opts) ->
	timer:sleep(10000).

onhello(Msg, _Opts) ->
	lager:debug("event from com.example.onhello ~p.", [Msg]),
	ok.
