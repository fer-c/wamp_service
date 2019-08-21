-module(wamp_service_trace_id).
%% from https://github.com/census-instrumentation/opencensus-erlang

-export([generate/0]).

-export_type([trace_id/0]).

-type trace_id() :: non_neg_integer().

-spec generate() -> trace_id().
generate() ->
    integer_to_binary(uniform(2 bsl 127 - 1)).

%% Before OTP-20 rand:uniform could not give precision higher than 2^56.
%% Here we do a compile time check for support of this feature and will
%% combine multiple calls to rand if on an OTP version older than 20.0
-ifdef(high_bit_uniform).
uniform(X) ->
    rand:uniform(X).
-else.
-define(TWO_POW_56, 2 bsl 55).

uniform(X) when X =< ?TWO_POW_56 ->
    rand:uniform(X);
uniform(X) ->
    R = rand:uniform(?TWO_POW_56),
    (uniform(X bsr 56) bsl 56) + R.
-endif.
