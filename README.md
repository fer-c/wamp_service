# wamp_service

__TODO: still needs generalization and refactoring especially opts.__

A boilerplate WAMP micro service infrastructure for developing basic micro service with WAMP support. This micro service registers to procedures: `com.example.add2` and `com.leapsight.echo`. The first one is intended to be used with crossbar example application.

This allows register callees and subscribers in a declarative way and abstract actual wamp complexity from service implementation. If more complex feature of the WAMP protocol is needed it should be handled by the service.

## Configuration
The micro service has several configurations in `sys.config`:

```erlang
 {wamp_service, [
                 {conf, [
                         {pool_name, wamp_service_worker_pool},
                         {pool_capacity, 1280000}, %% 16 * erlang:system_info(schedulers) * 10000
                         {pool_size, 128},
                         %% wamp opts
                         {hostname, "localhost"},
                         {port, 8080},
                         {realm, <<"realm1">>},
                         {encoding,  msgpack},
                         {callbacks, [
                                      {callee, <<"com.example.add2">>, {wamp_service_service, add}},
                                      {callee, <<"com.leapsight.echo">>, {wamp_service_service, echo}},
                                      {subscriber, <<"com.example.onhello">>, {wamp_service_service, onhello}}
                                     ]}
                        ]}
                ]
 }
```


The __pool options__ configure how load will be handled by the service using [sidejob](https://github.com/basho/sidejob).

The __wamp options__ are the usual connection options plus __callbacks__ definitions, for each callback it will be added a callee or subscriber with the given URI and the handler given by the tuple `{module, function}`.

## Build

    $ rebar3 compile

## Test

In order to test you must start a wamp broker, for example crossbar:

    $ mkdir example && cd example && crossbar init && crossbar start

In the Erlang shell start the micro service:

    $ rebar3 shell
    1> application:start(wamp_service).

To test the micro service and published procedures on the same shell or a new one:

    1> {ok, Con} = awre:start_client().
    2> {ok, SessionId, _RouterDetails} = awre:connect(Con, "localhost", 8080, <<"realm1">>, msgpack).
    3> awre:call(Con, [], <<"com.leapsight.echo">>, ["Hello wamp!"]).
    4> awre:call(Con, [], <<"com.example.add2">>, [1, 1]).
    5> awre:publish(Con, [], <<"com.example.onhello">>, ["Hello wamp!"]).


## Developing a New Service

In order to create a new service you should clone this demo [wamp_service_demo](https://gitlab.com/leapsight/wamp_service_demo) service and change the configuration. In the future we plan to create a rebar3 plugin to facilitate starting a new service.
