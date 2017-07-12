%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================


-module(wamp_service_call).

-behaviour(gen_server).

-export([init/1]).

init([_Opts]) ->
    {ok, #{}}.
