%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================
-module(wamp_service_utils).

-export([connect/4, reconnect/7, options/1, args/1]).

connect(Host, Port, Realm, Encoding) ->
    try
        {ok, Conn} = awre:start_client(),
        monitor(process, Conn),
        {ok, SessionId, _RouterDetails} = awre:connect(Conn, Host, Port, Realm, Encoding),
        %% and register procedures & subscribers
        _ = lager:info("Session started session_id=~p", [SessionId]),
        {ok, {Conn, SessionId}}
    catch
        Class:Reason ->
            _ = lager:error("Connection error class=~p reason=~p stacktarce=~p",
                            [Class, Reason, erlang:get_stacktrace()]),
            error
    end.

reconnect(Host, Port, Realm, Encoding, Backoff, Retries, Attempts) ->
    case Attempts =< Retries of
        false ->
            _ = lager:error("Failed to reconnect :-("),
            exit(wamp_connection_error);
        true ->
            {Time, Backoff1} = backoff:fail(Backoff),
            ok = lager:info("Reconnecting, attempt ~p of ~p (retry in ~ps) ...", [Attempts, Retries, Time/1000]),
            case connect(Host, Port , Realm, Encoding) of
                {ok, Res} ->
                    {ok, Res};
                error ->
                    _ = lager:info("Reconnection failed"),
                    timer:sleep(Time),
                    reconnect(Host, Port, Realm, Encoding, Backoff1, Retries, Attempts + 1)
            end
    end.

options(undefined) ->
    #{};
options(ArgsKw) when is_map(ArgsKw) ->
    ArgsKw.

args(undefined) ->
    [];
args(Args) when is_list(Args) ->
    Args;
args(Arg) ->
    [Arg].
