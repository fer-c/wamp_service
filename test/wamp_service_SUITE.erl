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
        dynamic_register,
        timeout_error_test,
        {group,
        parallel_echo},
        {group,
        circular},
        {group,
        unregister_register},
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
    {ok, _} = application:ensure_all_started(wamp_service),
    timer:sleep(2000),
    Config.

end_per_suite(_Config) ->
    application:stop(wamp_service).


echo_test(_) ->
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [Msg], _, _},
        wamp_service_peer:call(
            default, <<"com.example.echo">>, [Msg], #{}, #{}
        )
    ).


multiple_results_test(_) ->
    ?assertMatch(
        {ok, [1, 2, 3], _, _},
        wamp_service_peer:call(
            default, <<"com.example.multiple">>, [], #{}, #{}
        )
    ).


circular_test(_) ->
    %% Ref = rand:uniform(),
    Ref = rand:uniform(1 bsl 64),
    ?assertMatch(
        {ok, [Ref], _, _},
        wamp_service_peer:call(
            default, <<"com.example.circular">>, [Ref], #{}, #{}
        )
    ).

circular_service_error(_) ->
    ?assertMatch(
        {error, <<"com.magenta.error.internal_error">>, _, _, _},
        wamp_service_peer:call(
            default, <<"com.example.circular_service_error">>, [], #{}, #{}
        )
    ).

unknown_error_test(_) ->
    ?assertMatch(
        {error, <<"com.magenta.error.internal_error">>, _, _, _}, wamp_service_peer:call(
            default, <<"com.example.unknown_error">>, [], #{}, #{}
        )
    ).

notfound_error_test(_) ->
    ?assertMatch(
        {error, <<"com.magenta.error.not_found">>, _, _, _},
        wamp_service_peer:call(
            default, <<"com.example.notfound_error">>, [], #{}, #{}
        )
    ).

validation_error_test(_) ->
    Expected = <<"wamp.error.invalid_argument">>,
    Result = wamp_service_peer:call(
        default, <<"com.example.validation_error">>, [], #{}, #{}
    ),
    ?assertEqual(Expected, element(2, Result)).

service_error_test(_) ->
    ?assertMatch(
        {error, <<"com.magenta.error.internal_error">>, _, _, _}, wamp_service_peer:call(
            default, <<"com.example.service_error">>, [], #{}, #{}
        )
    ).

authorization_error_test(_) ->
    ?assertMatch(
        {error, <<"wamp.error.not_authorized">>, _, _, _},
        wamp_service_peer:call(
            default, <<"com.example.authorization_error">>, [], #{}, #{}
        )
    ).

timeout_error_test(_) ->
    ?assertMatch(
        {error, <<"wamp.error.timeout">>, _, _, _},
        wamp_service_peer:call(
            default, <<"com.example.timeout">>, [1000], #{}, #{}
        )
    ).

override_registered_procedure(_) ->
    %% Already Registered
    Uri = <<"com.example.echo">>,
    Fun = fun(_, _, _) -> {ok, [<<"new_echo">>], #{}, #{}} end,

    {error, {already_registered, Reg1}} = wamp_service_peer:register(
        default, Uri, #{}, Fun
    ),
    timer:sleep(100), %% wait for registration

    ?assertMatch(
        {ok, [<<"old_echo">>], _, _},
        wamp_service_peer:call(
            default, <<"com.example.echo">>, [<<"old_echo">>], #{}, #{}
        )
    ),
    ok = wamp_service_peer:unregister(default, Uri),

    {ok, Reg2} = wamp_service_peer:register(default, Uri, #{}, Fun),
    timer:sleep(100), %% wait for registration

    ?assertNotEqual(Reg1, Reg2),

    ?assertMatch(
        {ok, [<<"new_echo">>], _, _},
        wamp_service_peer:call(
            default, <<"com.example.echo">>, [<<"old_echo">>], #{}, #{}
        )
    ).


dynamic_register(_) ->
    {ok, _} = wamp_service_peer:register(
        default,
        <<"com.example.echo1">>,
        #{},
        fun(X, _, _) -> {ok, [X], #{}, #{}} end
    ),
    timer:sleep(100), %% wait for registration
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [Msg], _, _},
        wamp_service_peer:call(
            default, <<"com.example.echo1">>, [Msg], #{}, #{}
        )
    ).

parallel_echo_test(_) ->
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [Msg], _, _},
        wamp_service_peer:call(
            default, <<"com.example.echo">>, [Msg], #{}, #{}
        )
    ).

unregister_register_test(_) ->
    N = rand:uniform(100000),
    Uri = <<"com.example.echo", (list_to_binary(integer_to_list(N)))/binary>>,
    {ok, _} = wamp_service_peer:register(
        default,
        Uri,
        #{},
        fun(_, _, _) -> timer:sleep(500), {ok, [<<"pong">>], #{}, #{}} end
    ),
    timer:sleep(1000),
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [<<"pong">>], _, _},
        wamp_service_peer:call(default, Uri, [Msg], #{}, #{})
    ),
    ok = wamp_service_peer:unregister(default, Uri).

publish_test(_) ->
    ok = wamp_service_peer:publish(
        default, <<"com.example.onhello">>, [<<"Hello wamp!">>], #{}, #{}
    ),
    ok = wamp_service_peer:publish(
        default, <<"com.example.onadd">>, [1, 2], #{}, #{}
    ).

disconnect_test(_) ->
    whereis(wamp_caller) ! error, %% force reconnect
    whereis(wamp_dispatcher) ! error, %% force reconnect
    timer:sleep(100),
    ?assertMatch(
        {ok, [[1, 2, 3]], _, _},
        wamp_service_peer:call(
            default, <<"com.example.multiple">>, [], #{}, #{}
        )
    ).

long_call_test(_) ->
    ?assertMatch(
        {ok, _, _, _},
        wamp_service_peer:call(
            default, <<"com.example.timeout">>, [10000], #{}, #{timeout => 20000}
        )
    ),
    ?assertMatch(
        {error, <<"wamp.error.timeout">>, _, _, _},
        wamp_service_peer:call(
            default, <<"com.example.timeout">>, [30000], #{}, #{timeout => 20000}
        )
    ).
