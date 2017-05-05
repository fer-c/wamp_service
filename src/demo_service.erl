-module(demo_service).

-export([add/1]).
-export([echo/1]).


add({{invocation, RequestId, _RpcId, _Details, [A, B], _ArgumentsKw}, #{con := Con}}) ->
	lager:info("add called, sent ~p + ~p.", [A, B]),
	ok = awre:yield(Con, RequestId, [], [A + B]).


echo({{invocation, RequestId, _RpcId, _Details, [Msg], _ArgumentsKw}, #{con := Con}}) ->
	lager:info("echo called, sent ~p.", [Msg]),
	ok = awre:yield(Con, RequestId, [], [Msg]).