-module(demo_service).

-export([add/3]).
-export([echo/2]).

add(A, B, _Opts) ->
	lager:debug("add called, sent ~p + ~p.", [A, B]),
	A + B.


echo(Msg, _Opts) ->
	lager:debug("echo called, sent ~p.", [Msg]),
	Msg.
