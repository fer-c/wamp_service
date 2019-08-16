-module(wamp_service_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

groups() ->
    [
     {circular, [parallel], lists:map(fun(_) -> circular_test end, lists:seq(1, 1000))},
     {parallel_echo, [parallel], lists:map(fun(_) -> parallel_echo_test end, lists:seq(1, 1000))},
     {unregister_register, [parallel], lists:map(fun(_) -> unregister_register_test end, lists:seq(1, 50))}
    ].

all() ->
    [
        echo_test,
        multiple_results_test,
        circular_service_error,
        unknown_error_test,
        notfound_error_test,
        validation_error_test,
        service_error_test,
        authorization_error_test,
        maybe_error_no_procedure_test,
        maybe_error_internal_error_test,
        maybe_error_success_test,
        dynamic_register,
        timeout_error_test,
        {group, circular},
        {group, parallel_echo},
        {group, unregister_register},
        override_registered_procedure,
        publish_test,
        disconnect_test,
        long_call_test
    ].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lager),
    lager_common_test_backend:bounce(debug),
    {ok, _} = application:ensure_all_started(wamp_service),
    timer:sleep(2000),
    Config.

end_per_suite(_Config) ->
    application:stop(wamp_service).


echo_test(_) ->
    Msg = <<"Hello, world!">>,
    {ok, [Msg], #{}, #{}} = wamp_service:call(<<"com.example.echo">>, [Msg], #{}, #{}).

multiple_results_test(_) ->
    {ok, [[1, 2, 3]], #{}, #{}} = wamp_service:call(<<"com.example.multiple">>, [], #{}, #{}).


circular_test(_) ->
    Ref = rand:uniform(),
    {ok, [Ref], #{}, #{}} = wamp_service:call(<<"com.example.circular">>, [Ref], #{}, #{}).

circular_service_error(_) ->
    {error, <<"com.magenta.error.internal_error">>, _, _, _} =
        wamp_service:call(<<"com.example.circular_service_error">>, [], #{}, #{}).

unknown_error_test(_) ->
    {error, <<"com.magenta.error.internal_error">>, _, _, _} =
        wamp_service:call(<<"com.example.unknown_error">>, [], #{}, #{}).

notfound_error_test(_) ->
    {error, <<"com.magenta.error.not_found">>, _, _, _} =
        wamp_service:call(<<"com.example.notfound_error">>, [], #{}, #{}).

validation_error_test(_) ->
    {error, <<"wamp.error.invalid_argument">>, _, _, _} =
        wamp_service:call(<<"com.example.validation_error">>, [], #{}, #{}).

service_error_test(_) ->
    {error, <<"com.magenta.error.internal_error">>, _, _, _} =
        wamp_service:call(<<"com.example.service_error">>, [], #{}, #{}).

authorization_error_test(_) ->
    {error, <<"wamp.error.not_authorized">>, _, _, _} =
        wamp_service:call(<<"com.example.authorization_error">>, [], #{}, #{}).

timeout_error_test(_) ->
    {error, <<"com.magenta.error.timeout">>, _, _, _} =
        wamp_service:call(<<"com.example.timeout">>, [], #{}, #{}).


maybe_error_internal_error_test(_) ->
    {error, <<"com.magenta.error.internal_error">>, _, _, _} =
        wamp_service:call(<<"com.example.service_error">>, [], #{}).

maybe_error_no_procedure_test(_) ->
    {error, <<"wamp.error.no_such_procedure">>, _, _, _} = wamp_service:call(<<"com.example.error">>, [], #{}, #{}).

maybe_error_success_test(_) ->
    Msg = <<"Hello, world!">>,
    {ok, [Msg], #{}, #{}} =
        wamp_service:call(<<"com.example.echo">>, [Msg], #{}, #{}).

override_registered_procedure(_) ->
    ok = wamp_service:register(procedure, <<"com.example.echo">>, fun(_, _, _) -> <<"new_echo">> end),
    timer:sleep(100), %% wait for registration
    {ok, [<<"new_echo">>], #{}, #{}} = wamp_service:call(<<"com.example.echo">>, [<<"old_echo">>], #{}, #{}).

dynamic_register(_) ->
    ok = wamp_service:register(procedure, <<"com.example.echo1">>, fun(X, _, _) -> X end),
    timer:sleep(100), %% wait for registration
    Msg = <<"Hello, world!">>,
    {ok, [Msg], #{}, #{}} = wamp_service:call(<<"com.example.echo1">>, [Msg], #{}, #{}).

parallel_echo_test(_) ->
    Msg = <<"Hello, world!">>,
    {ok, [Msg], #{}, #{}} = wamp_service:call(<<"com.example.echo">>, [Msg], #{}, #{}).

unregister_register_test(_) ->
    N = rand:uniform(100000),
    Uri = <<"com.example.echo", (list_to_binary(integer_to_list(N)))/binary>>,
    ok = wamp_service:register(procedure, Uri, fun(_, _, _) -> timer:sleep(500), <<"pong">> end),
    timer:sleep(1000),
    Msg = <<"Hello, world!">>,
    {ok, [<<"pong">>], #{}, #{}} = wamp_service:call(Uri, [Msg], #{}, #{}),
    ok = wamp_service:unregister(Uri).

publish_test(_) ->
    ok = wamp_service:publish(<<"com.example.onhello">>, [<<"Hello wamp!">>], #{}, #{}),
    ok = wamp_service:publish(<<"com.example.onadd">>, [1, 2], #{}, #{}).

disconnect_test(_) ->
    whereis(wamp_caller) ! error, %% force reconnect
    whereis(wamp_dispatcher) ! error, %% force reconnect
    timer:sleep(200),
    {ok, [[1, 2, 3]], #{}, #{}} = wamp_service:call(<<"com.example.multiple">>, [], #{}, #{}).

long_call_test(_) ->
    {ok, _, _, _} = wamp_service:call(<<"com.example.timeout">>, [], #{}, #{timeout => 20000}).
