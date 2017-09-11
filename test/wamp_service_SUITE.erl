-module(wamp_service_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

groups() ->
    [{circular, [parallel, {repeat, 100}], [circular_test]}].

all() ->
    [echo_test, error_test, timeout_test, maybe_error_error_test,
     maybe_error_success_test, {group, circular}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_suite(Config) ->
    application:ensure_all_started(wamp_service),
    Config.

end_per_suite(_Config) ->
    application:stop(wamp_service).

circular_test(_) ->
    Ref = rand:uniform(),
    Ref = wamp_service:call(<<"com.example.circular">>, [Ref], #{}).

echo_test(_) ->
    Msg = <<"Hello, world!">>,
    Msg = wamp_service:call(<<"com.example.echo">>, [Msg], #{}).

error_test(_) ->
    {error, <<"wamp.error.unknown_error">>, _} = wamp_service:call(<<"com.example.error">>, [], #{}).

timeout_test(_) ->
    {error, <<"wamp.error.timeout">>, _} = wamp_service:call(<<"com.example.timeout">>, [], #{}).

maybe_error_error_test(_) ->
    ?assertError({error, _, _},
                 wamp_service:maybe_error(wamp_service:call(<<"com.example.error">>, [], #{}))).

maybe_error_success_test(_) ->
    Msg = <<"Hello, world!">>,
    Msg = wamp_service:maybe_error(wamp_service:call(<<"com.example.echo">>, [Msg], #{})).
