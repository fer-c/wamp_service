# wamp_service

> This library wraps an existing (incomplete) WAMP client implementation and adds some required features. It is intended to be replaced by a proper WAMP client implementation in Erlang.

# Versions 0.6.0 and above


## Changes versus previous versions

### Summary

* New module `wamp_service_peer` to replace `wamp_service`
* Allows pools of WAMP Peers, each one with its own TCP connection to the router
* The Handler API has been completely redesigned
* The configuration has been completely redesigned

## Handler API
The new handler API follows the WAMP APIs.

All callbacks should have at least two arguments (`KWArgs` and `Opts`).

```erlang
-type callback  ::  {module(), FunctionName :: atom()}
                    | function(
        (KWArgs :: map(), Opts :: map()) ->
            wamp_return() | wamp_error();
        (Arg1 :: any(), ..., ArgN :: any(), KWArgs :: map(), Opts :: map()) ->
            wamp_return() | wamp_error()
    )
```

Return types
```erlang
-type wamp_result()             ::  {
                                        ok,
                                        Args :: list(),
                                        KWArgs :: map(),
                                        Details :: map()
                                    }.

-type wamp_error()              ::  {
                                        error,
                                        Uri :: binary(),
                                        Args :: list(),
                                        KWArgs :: map(),
                                        Details :: map()
                                    }.
```

## Configuration

### Type Spec and structure

```erlang
{wamp_service, [
    {routers, Routers :: map(Name :: atom() => Router :: router())},
    {defaults, #{
        router => RouterName :: atom(),
        caller => #{
            features => caller_features(),
            options => caller_options()
        },
        callee => #{
            features => caller_features(),
            options => caller_options()
        },
        subscriber => #{
            features => caller_features(),
            options => caller_options()
        },
        publisher => #{
            features => caller_features(),
            options => caller_options()
        }
    }},
    {peers, #{
        PeerName :: atom() => Peer :: peer()
    }}

]}
```

#### Peer
```erlang
#{
    router => RouterName :: atom(),
    pool_size => integer(),
    pool_type => hash | round_robin |direct | random,
    roles => #{
        caller => #{},
        publisher => #{},
        callee => #{
            features => #{},
            registrations => #{URI :: binary() => registration()}
        },
        callee => #{
            features => #{},
            subscriptions => #{URI :: binary() => subscription()}
        }
    }
```

### Complete example
```erlang
[
    %% service conf
    {wamp_service, [
        {routers, #{
            bondy => #{
                hostname => "localhost",
                port => 18082,
                realm => <<"com.magenta.test">>,
                encoding => erlbin,
                reconnect => true,
                reconnect_max_retries => 10,
                reconnect_backoff_min => 500,
                reconnect_backoff_max => 120000,
                reconnect_backoff_type => jitter
            }
        }},
        {defaults, #{
            router => bondy,
            caller => #{
                features => #{
                    progressive_call_results => false,
                    progressive_calls => false,
                    call_timeout => true,
                    call_canceling => false,
                    caller_identification=> true,
                    call_retries => true
                },
                options => #{
                    timeout => 15000,
                    disclose_me => true
                }
            },
            callee => #{
                features => #{
                    progressive_call_results => false,
                    progressive_calls => false,
                    call_timeout => true,
                    call_canceling => false,
                    caller_identification => true,
                    call_trustlevels => true,
                    registration_revocation => true,
                    session_meta_api => true,
                    pattern_based_registration => true,
                    shared_registration => true,
                    sharded_registration => true
                },
                options => #{
                    disclose_caller => true,
                    invoke => roundrobin
                }
            },
            publisher => #{
                features => #{
                    message_retention => true,
                    publisher_exclusion => true,
                    publisher_identification => true,
                    subscriber_blackwhite_listing => true
                }
            },
            subscriber => #{
                features => #{
                    event_history => false,
                    pattern_based_subscription => true,
                    publication_trustlevels => true,
                    publisher_identification => true,
                    sharded_subscription => true
                }
            }
        }},
        {peers, #{
            default => #{
                router => bondy,
                pool_size => 3,
                pool_type => hash,
                roles => #{
                    caller => #{},
                    publisher => #{},
                    callee => #{
                        features => #{},
                        registrations => #{
                            <<"com.example.add2">> => #{
                                options => #{
                                    disclose_caller => true,
                                    invoke => roundrobin
                                },
                                handler => {wamp_service_example, add}
                            },
                            <<"com.example.echo">> =>#{
                                handler => {wamp_service_example, echo}
                            },
                            <<"com.example.multiple">> =>#{
                                handler => {wamp_service_example, multiple_results}
                            },
                            <<"com.example.circular">> =>#{
                                handler => {wamp_service_example, circular}
                            },
                            <<"com.example.circular_service_error">> =>#{
                                handler => {wamp_service_example, circular_service_error}
                            },
                            <<"com.example.unknown_error">> =>#{
                                handler => {wamp_service_example, unknown_error}
                            },
                            <<"com.example.notfound_error">> =>#{
                                handler => {wamp_service_example, notfound_error}
                            },
                            <<"com.example.validation_error">> =>#{
                                handler => {wamp_service_example, validation_error}
                            },
                            <<"com.example.service_error">> =>#{
                                handler => {wamp_service_example, service_error}
                            },
                            <<"com.example.authorization_error">> =>#{
                                handler => {wamp_service_example, authorization_error}
                            },
                            <<"com.example.timeout">> =>#{
                                handler => {wamp_service_example, timeout}
                            }
                        }
                    },
                    subscriber => #{
                        features => #{},
                        subscriptions => #{
                            <<"com.example.onhello">> =>#{
                                handler => {wamp_service_example, onhello}
                            },
                            <<"com.example.onadd">> => #{
                                handler => {wamp_service_example, onadd}
                            }
                        }
                    }
                }
            }
        }}
    ]},

    {awre, [
        {erlbin_number, 15}
    ]},

    {kernel, [
        {logger, [
          {handler, default, logger_std_h,
            #{formatter => {log_formatter, #{single_line => true, depth => 10 }}}
          }
        ]},
        {logger_level, info}
    ]}
].
```



# Version previous to 0.5.2
[DEPRECATED]

## Configuration
The micro service has several configurations in `sys.config`:

```erlang
 %% service conf
 {wamp_service,
  [
   {callee_dispatcher,
    [
     %% wamp opts
     {hostname, "localhost"},
     {port, 18082},
     {realm, <<"com.magenta.test">>},
     {encoding, json},
     {reconnect, true},
     %% service callbacks
     {callbacks,
      [
       {procedure, <<"com.example.add2">>, {wamp_service_example, add}},
       {procedure, <<"com.example.echo">>, {wamp_service_example, echo}},
       {procedure, <<"com.example.multiple">>, {wamp_service_example, multiple_results}},
       {procedure, <<"com.example.circular">>, {wamp_service_example, circular}},
       {procedure, <<"com.example.circular_service_error">>, {wamp_service_example, circular_service_error}},
       {procedure, <<"com.example.unknown_error">>, {wamp_service_example, unknown_error}},
       {procedure, <<"com.example.notfound_error">>, {wamp_service_example, notfound_error}},
       {procedure, <<"com.example.validation_error">>, {wamp_service_example, validation_error}},
       {procedure, <<"com.example.service_error">>, {wamp_service_example, service_error}},
       {procedure, <<"com.example.authorization_error">>, {wamp_service_example, authorization_error}},
       {procedure, <<"com.example.timeout">>, {wamp_service_example, timeout}},
       {subscription, <<"com.example.onhello">>, {wamp_service_example, onhello}},
       {subscription, <<"com.example.onadd">>, {wamp_service_example, onadd}}
      ]}
    ]
   },
   {caller_service,
    [
     %% wamp opts
     {hostname, "localhost"},
     {port, 18082},
     {realm, <<"com.magenta.test">>},
     {encoding, json},
     {reconnect, true}
    ]
   }
  ]},
```

The __worker args__ are the usual connection options plus __service callbacks__ definitions, for each callback it will be added a procedure or subscription with the given URI and the handler given by the tuple `{module, function}. Finally the _reconnect options_ are the attempts to retry to reconnect and initial exponential backoff.

## Build

    $ rebar3 compile

## Test

In order to test you must start a wamp broker, for example [bondy](https://gitlab.com/leapsight/bondy) for testing.

Or using docker

    $ docker run --rm -it -p 18080:18080 -p 18081:18081 -p 18082:18082 --name bondy registry.gitlab.com/leapsight/bondy:latest

Start the erlang shell:

```shell
rebar3 shell
```

In the Erlang shell start the micro service:

    application:start(wamp_service).

To test the micro service and published procedures on the same shell or a new one:

    wamp_service:call(<<"com.example.echo">>, ["Hello wamp!"], #{<<"security">> => #{<<"groups">> => [<<"admin">>]}}).
    wamp_service:call(<<"com.example.add2">>, [1, 1], #{}).
    wamp_service:call(<<"com.example.error">>, [], #{}). % error test
    wamp_service:publish(<<"com.example.onhello">>, [<<"Hello wamp!">>], #{}).
    wamp_service:publish(<<"com.example.onadd">>, [1, 2], #{}).

The `call` function return either the result or an error result, see `maybe_call` for variants
automatically raising an `error()`.

You can also register or unregister procedure or subscription dynamically in the following way:

    wamp_service:unregister(<<"com.example.echo">>).
    wamp_service:register(procedure, <<"com.example.echo">>, fun(X, _Opts) -> X end).

## Developing a new Service

In order to create a new service you should use the rebar3 template [basic_service_template](https://gitlab.com/leapsight-lojack/basic_service_template).

## Volume test

```erlang
application:start(wamp_service).
lists:foreach(fun(N) ->
                spawn(fun() ->
                        T1 = erlang:system_time(millisecond),
                        N1 = N + 1,
                        {ok, N1} = wamp_service:call(<<"com.example.add2">>, [N, 1], #{<<"trace_id">> => N}),
                        io:format("~p -> ~p~n", [N, erlang:system_time(millisecond) - T1])
                      end)
             end, lists:seq(1, 1000)).
```
