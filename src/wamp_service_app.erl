%% =============================================================================
%%  wamp_service_app.erl -
%%
%%  Copyright (c) 2016-2021 Leapsight. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================
-module(wamp_service_app).
-behaviour(application).


-export([start/2]).
-export([stop/1]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc Initialises plum_db configuration
%% @end
%% -----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ok = wamp_service_config:init(),
    case wamp_service_sup:start_link() of
        {ok, _} = OK ->
            OK;
        {error, _} = Error ->
            Error
    end.


%% -----------------------------------------------------------------------------
%% @doc Initialises plum_db configuration
%% @end
%% -----------------------------------------------------------------------------
stop(_State) ->
    ok.
