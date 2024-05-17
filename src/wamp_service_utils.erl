%% =============================================================================
%% Copyright (C) NGINEO LIMITED 2011 - 2016. All rights reserved.
%% =============================================================================
-module(wamp_service_utils).


-export([args/1]).
-export([init_backoff/1]).
-export([init_backoff/2]).
-export([options/1]).
-export([reconnect_backoff_max/1]).
-export([reconnect_backoff_max/2]).
-export([reconnect_backoff_min/1]).
-export([reconnect_backoff_min/2]).
-export([reconnect_backoff_type/1]).
-export([reconnect_backoff_type/2]).
-export([reconnect_retries/1]).
-export([reconnect_retries/2]).
-export([reconnect/1]).
-export([reconnect/2]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
options(undefined) ->
    #{};

options(ArgsKw) when is_map(ArgsKw) ->
    ArgsKw.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
args(undefined) ->
    [];

args(Args) when is_list(Args) ->
    Args;

args(Arg) ->
    [Arg].


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init_backoff(boolean(), proplist:proplist()) -> backoff:backoff() | undefined.

init_backoff(false, _Opts) ->
    undefined;

init_backoff(true, Opts) ->
    init_backoff(Opts).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init_backoff(proplist:proplist()) -> backoff:backoff().

init_backoff(Opts) ->
    Min = reconnect_backoff_min(Opts),
    Max = reconnect_backoff_max(Opts),
    Type = reconnect_backoff_type(Opts),
    backoff:type(backoff:init(Min, Max), Type).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect(proplist:proplist()) -> non_neg_integer().

reconnect(Opts) ->
    reconnect(Opts, false).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect(proplist:proplist(), boolean()) -> boolean().

reconnect(Opts, Default) ->
    proplists:get_value(reconnect, Opts, Default).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_retries(proplist:proplist()) -> non_neg_integer().

reconnect_retries(Opts) ->
    reconnect_retries(Opts, 10).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_retries(proplist:proplist(), non_neg_integer()) -> non_neg_integer().

reconnect_retries(Opts, Default) ->
    %% to be compatible with previuos versions
    Retries = proplists:get_value(retries, Opts, Default),
    proplists:get_value(reconnect_retries, Opts, Retries).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_backoff_min(proplist:proplist()) -> non_neg_integer().

reconnect_backoff_min(Opts) ->
    reconnect_backoff_min(Opts, 500).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_backoff_min(proplist:proplist(), non_neg_integer()) -> non_neg_integer().

reconnect_backoff_min(Opts, Default) ->
    %% to be compatible with previuos versions
    InitBackoff = proplists:get_value(backoff, Opts, Default),
    proplists:get_value(reconnect_backoff_min, Opts, InitBackoff).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_backoff_max(proplist:proplist()) -> non_neg_integer().

reconnect_backoff_max(Opts) ->
    reconnect_backoff_max(Opts, 60000).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_backoff_max(proplist:proplist(), non_neg_integer()) -> non_neg_integer().

reconnect_backoff_max(Opts, Default) ->
    proplists:get_value(reconnect_backoff_max, Opts, Default).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_backoff_type(proplist:proplist()) -> non_neg_integer().

reconnect_backoff_type(Opts) ->
    reconnect_backoff_max(Opts, jitter).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec reconnect_backoff_type(proplist:proplist(), jitter | normal) -> non_neg_integer().

reconnect_backoff_type(Opts, Default) ->
    proplists:get_value(reconnect_backoff_type, Opts, Default).