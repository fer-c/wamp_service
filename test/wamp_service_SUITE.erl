-module(wamp_service_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

groups() ->
    [{circular, [parallel, {repeat, 10}], [circular_test]}].

all() ->
    [echo_test, circular_service_error, unknown_error_test, notfound_error_test,
     validation_error_test, service_error_test, authorization_error_test,
     maybe_error_no_procedure_test, maybe_error_internal_error_test,
     maybe_error_success_test, already_registered_error, dynamic_register,
     invalid_fun_error, unregister_register, {group, circular}, timeout_error_test].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lager),
    lager_common_test_backend:bounce(debug),
    {ok, _} = application:ensure_all_started(wamp_service),
    Config.

end_per_suite(_Config) ->
    application:stop(wamp_service).


echo_test(_) ->
    Msg = <<"Hello, world!">>,
    {ok, Msg} = wamp_service:call(<<"com.example.echo">>, [Msg], #{}).

circular_test(_) ->
    Ref = rand:uniform(),
    {ok, Ref} = wamp_service:call(<<"com.example.circular">>, [Ref], #{}).

circular_service_error(_) ->
    {error, <<"com.magenta.error.internal_error">>, _} = wamp_service:call(<<"com.example.circular_service_error">>, [], #{}).

unknown_error_test(_) ->
    {error, <<"com.magenta.error.unknown_error">>, _} = wamp_service:call(<<"com.example.unknown_error">>, [], #{}).

notfound_error_test(_) ->
    {error, <<"com.magenta.error.not_found">>, _} = wamp_service:call(<<"com.example.notfound_error">>, [], #{}).

validation_error_test(_) ->
    {error, <<"wamp.error.invalid_argument">>, _} = wamp_service:call(<<"com.example.validation_error">>, [], #{}).

service_error_test(_) ->
    {error, <<"com.magenta.error.internal_error">>, _} = wamp_service:call(<<"com.example.service_error">>, [], #{}).

authorization_error_test(_) ->
    {error, <<"wamp.error.not_authorized">>, _} = wamp_service:call(<<"com.example.authorization_error">>, [], #{}).

timeout_error_test(_) ->
    {error, <<"com.magenta.error.timeout">>, _} = wamp_service:call(<<"com.example.timeout">>, [], #{}).


maybe_error_internal_error_test(_) ->
    ?assertError({error, <<"com.magenta.error.internal_error">>, _},
                 wamp_service:maybe_error(wamp_service:call(<<"com.example.service_error">>, [], #{}))).

maybe_error_no_procedure_test(_) ->
    ?assertError({error, <<"wamp.error.no_such_procedure">>, _},
                 wamp_service:maybe_error(wamp_service:call(<<"com.example.error">>, [], #{}))).

maybe_error_success_test(_) ->
    Msg = <<"Hello, world!">>,
    {ok, Msg} = wamp_service:maybe_error(wamp_service:call(<<"com.example.echo">>, [Msg], #{})).

already_registered_error(_) ->
    {error, _} = wamp_service:register(procedure, <<"com.example.echo">>, fun(X) -> X end).

dynamic_register(_) ->
    ok = wamp_service:register(procedure, <<"com.example.echo1">>, fun(X, _) -> X end),
    Msg = <<"Hello, world!">>,
    {ok, Msg} = wamp_service:maybe_error(wamp_service:call(<<"com.example.echo1">>, [Msg], #{})).

invalid_fun_error(_) ->
    {error, _} = wamp_service:register(procedure, <<"com.example.echo2">>, {cosa, cosa}).

unregister_register(_) ->
    ok = wamp_service:unregister(<<"com.example.echo1">>),
    ok = wamp_service:register(procedure, <<"com.example.echo1">>, fun(_, _) -> <<"pong">> end),
    Msg = <<"Hello, world!">>,
    {ok, <<"pong">>} = wamp_service:maybe_error(wamp_service:call(<<"com.example.echo1">>, [Msg], #{})).
