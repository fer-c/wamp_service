%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================


-module(wamp_service_instr).

-export([ping/1, log_level/2]).

-spec ping(map()) -> binary().
ping(_ArgsKw) ->
    <<"pong">>.

-spec log_level(binary(), map()) -> undefined.
log_level(Level, _ArgsKw) ->
    L = binary_to_existing_atom(Level, utf8),
    logger:set_application_level(wamp_service, L),
    undefined.
