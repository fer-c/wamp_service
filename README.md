# wamp_service

__TODO: still needs generalization and refactoring especially opts.__

A demo wamp microservice. This microservice registers to procedures: `com.example.add2` and `com.leapsight.echo`. The first one is intended to be used with crossbar example application.

## Config
The micro service has several configurations in `sys.config`:

```erlang
    {wamp_service, [
                {conf, [
                    {pool_name, wamp_service_worker_pool},
                    {pool_capacity, 1280000}, %% 16 * erlang:system_info(schedulers) * 10000
                    {pool_size, 128}, %% 16 * erlang:system_info(schedulers)
                    %% wamp opts
                    {hostname, "localhost"},
                    {port, 8080},
                    {realm, <<"realm1">>},
                    {encoding,  msgpack},
                    {services, [
                            {<<"com.example.add2">>, {demo_service, add}},
                            {<<"com.leapsight.echo">>, {demo_service, echo}}
                            ]}
                ]}
            ]
    }
```


The __pool options__ configure how load will be handled by the service using [sidejob](https://github.com/basho/sidejob). 

The __wamp options__ are the usual connection options plus __services__, for each service of the list it will be added as callee or subscriber with the given URI and the handler given by the tuple `{module, function}`.

## Build

    $ rebar3 compile

## Test

In order to test you must start a wamp broker, for example crossbar:

    $ mkdir example && cd example && crossbar init && crossbar start

In the erlang shell start the micro service:

    $ erl
    1> application:start(wamp_service).

To test the micro service and published procedures:

    1> {ok, Con} = awre:start_client().
    2> {ok, SessionId, _RouterDetails} = awre:connect(Con, "localhost", 8080, <<"realm1">>, msgpack).
    3> awre:call(Con, [], <<"com.leapsight.echo">>, ["Hello wamp"]).
    4> awre:call(Con, [], <<"com.example.add2">>, [1, 1]).
