%%%-------------------------------------------------------------------
%% @doc wamp public API
%% @end
%%%-------------------------------------------------------------------

-module(wamp_service_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Res = wamp_service_sup:start_link(),
    register_services(),
    Res.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
register_services() ->
    {_, Host} = service_and_host(),
    Ping = <<"com.magenta.", Host/binary, ".ping">>,
    LogLevel = <<"com.magenta.", Host/binary, ".log_level">>,
    wamp_service:register(wamp_sessions, procedure, Ping, fun wamp_service_instr:ping/1, [<<"admin">>]),
    wamp_service:register(wamp_sessions, procedure, LogLevel, fun wamp_service_instr:log_level/2, [<<"admin">>]).

service_and_host() ->
    [Service, Host] = binary:split(atom_to_binary(node(), utf8), [<<"@">>]),
    {Service, Host}.
