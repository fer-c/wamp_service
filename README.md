# wamp_service

__TODO: still needs generalization and refactoring especially opts.__

A boilerplate WAMP micro service infrastructure for developing basic micro service with WAMP support. This micro service registers to procedures: `com.example.add2` and `com.leapsight.echo`. The first one is intended to be used with crossbar example application.

This allows register procedures and subscriptions in a declarative way and abstract actual wamp complexity from service implementation. If more complex feature of the WAMP protocol is needed it should be handled by the service.

## Configuration
The micro service has several configurations in `sys.config`:

```erlang
 {wamp_service, [
                 {session_pools, [
                                  {wamp_sessions, [
                                                   %% pool args
                                                   {size, 10},
                                                   {max_overflow, 20}
                                                  ], [
                                                      %% worker args
                                                      %% wamp opts
                                                      {hostname, "localhost"},
                                                      {port, 18082},
                                                      {realm, <<"magenta">>},
                                                      {encoding,  msgpack},
                                                      %% reconnect options
                                                      {backoff, 100},
                                                      {retries, 10},
                                                      %% service callbacks
                                                      {callbacks, [
                                                                   {procedure, <<"com.example.add2">>, {wamp_service_example, add}},
                                                                   {procedure, <<"com.example.echo">>, {wamp_service_example, echo}, [<<"admin">>]},
                                                                   {subscription, <<"com.example.onhello">>, {wamp_service_example, onhello}}
                                                                  ]}
                                                     ]}
                                 ]}
                ]},
```


The __pool args__ configure how load will be handled by the service using a pool of sessions.

The __worker args__ are the usual connection options plus __service callbacks__ definitions, for each callback it will be added a procedure or subscription with the given URI and the handler given by the tuple `{module, function}. Finally the _reconnect options_ are the attempts to retry to reconnect and initial exponential backoff.

## Build

    $ rebar3 compile

## Test

In order to test you must start a wamp broker, for example [bondy](https://gitlab.com/leapsight/bondy) for testing.

Start the erlang shell:

    $ rebar3 auto

In the Erlang shell start the micro service:

    application:start(wamp_service).

To test the micro service and published procedures on the same shell or a new one:

    wamp_service:call(<<"com.example.echo">>, ["Hello wamp!"], #{<<"security">> => #{<<"scope">> => <<"admin">>}}).
    wamp_service:call(<<"com.example.add2">>, [1, 1], #{}).
    wamp_service:publish(<<"com.example.onhello">>, <<"Hello wamp!">>, #{}).


## Developing a new Service

In order to create a new service you should use the rebar3 template [basic_service_template](https://gitlab.com/leapsight-lojack/basic_service_template).
