%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================
-module(wamp_service_utils).

-export([options/1, args/1, map_to_list/1]).

options(undefined) ->
    #{};
options(ArgsKw) when is_map(ArgsKw) ->
    ArgsKw.

args(undefined) ->
    [];
args(Args) when is_list(Args) ->
    Args;
args(Arg) ->
    [Arg].

map_to_list(M) when is_map(M) ->
    lists:map(fun({K, V}) -> {K, map_to_list(V)} end, maps:to_list(M));
map_to_list(V) ->
    V.
