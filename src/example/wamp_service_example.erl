%% Demo service
-module(wamp_service_example).

-export([add/3]).
-export([echo/2]).
-export([error/1]).
-export([onhello/2]).

add(A, B, _Opts) ->
	lager:debug("add called, sent ~p + ~p.", [A, B]),
	A + B.


echo(Msg, _Opts) ->
	lager:debug("echo called, sent ~p.", [Msg]),
	Msg.

error(_Opts) ->
	1 = 2.

onhello(Msg, _Opts) ->
	lager:debug("event from com.example.onhello ~p.", [Msg]),
	ok.
