# wamp_service

__TODO: still needs generalization and refactoring especially opts.__

A boilerplate WAMP micro service infrastructure for developing basic micro service with WAMP support. This micro service registers to procedures: `com.example.add2` and `com.leapsight.echo`. The first one is intended to be used with crossbar example application.

This allows register procedures and subscriptions in a declarative way and abstract actual wamp complexity from service implementation. If more complex feature of the WAMP protocol is needed it should be handled by the service.

## Configuration
The micro service has several configurations in `sys.config`:

```erlang
 {wamp_service, [
                 {pool_name, wamp_service_worker_pool},
                 {pool_capacity, 1280000}, %% 16 * erlang:system_info(schedulers) * 10000
                 {pool_size, 128},
                 %% wamp opts
                 {hostname, "localhost"},
                 {port, 8080},
                 {realm, <<"realm1">>},
                 {encoding,  msgpack},
                 {callbacks, [
                              {procedure, <<"com.example.add2">>, {wamp_service_service, add}, [<<"admin">>]},
                              {subscription, <<"com.leapsight.echo">>, {wamp_service_service, echo}},
                              {subscription, <<"com.example.onhello">>, {wamp_service_service, onhello}}
                             ]}
                ]
 },
```


The __pool options__ configure how load will be handled by the service using [sidejob](https://github.com/basho/sidejob).

The __wamp options__ are the usual connection options plus __callbacks__ definitions, for each callback it will be added a procedure or subscription with the given URI and the handler given by the tuple `{module, function}`.

## Build

    $ rebar3 compile

## Test

In order to test you must start a wamp broker, for example crossbar:

    $ mkdir example && cd example && crossbar init && crossbar start

In the Erlang shell start the micro service:

    $ rebar3 shell
    1> application:start(wamp_service).

To test the micro service and published procedures on the same shell or a new one:

    wamp_call:start().
    wamp_call:call(<<"com.example.echo">>, ["Hello wamp!"], #{<<"security">> => #{<<"scope">> => <<"admin">>}}).
    wamp_call:call(<<"com.example.add2">>, [1, 1], #{}).
    wamp_call:publish(<<"com.example.onhello">>, ["Hello wamp!"], #{}).


## Developing a new Service

In order to create a new service you should use the rebar3 template [basic_service_template](https://gitlab.com/leapsight-lojack/basic_service_template).
