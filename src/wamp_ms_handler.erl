%%% File        : worker_test.erl
%%% Author      : Federico Repond
%%% Description : 
%%% Created     : 29 Apr 2017 by Federico Repond
-module(wamp_ms_handler).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link(Opts) ->
	gen_server:start_link(?MODULE, [Opts], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Opts]) ->
	%% init worker pool
	PoolName = proplists:get_value(pool_name, Opts),
	Capacity = proplists:get_value(pool_capacity, Opts),
	Size = proplists:get_value(pool_size, Opts),
	sidejob:new_sharded_resource(PoolName, wamp_ms_worker, Capacity, Size),
	%% connect to wamp broker
	Host = proplists:get_value(hostname, Opts),
	Port = proplists:get_value(port, Opts),
	Realm = proplists:get_value(realm, Opts),
	Encoding = proplists:get_value(encoding, Opts),
	{ok, Con} = awre:start_client(), 
	{ok, SessionId, _RouterDetails} = awre:connect(Con, Host, Port, Realm, Encoding),
	lager:info("done (~p).", [SessionId]),
	%% and register procedure
	Services = register_services(Con, Opts),
	{ok, #{con => Con, session => SessionId, services => Services, pool_name => PoolName}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) ->
%%                {reply, Reply, State} |
%%                {reply, Reply, State, Timeout} |
%%                {noreply, State} |
%%                {noreply, State, Timeout} |
%%                {stop, Reason, Reply, State} |
%%                {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_, _, State) ->
	{noreply,State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_, State) ->
	{noreply, State}.



%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({awre, {invocation, RequestId, RpcId, _Details, _Args, _ArgumentsKw} = Invocation}, 
			#{services := Services, con := Con, pool_name := PoolName} = State) ->
	lager:info("been called ~p ... will just handle it ...", [Invocation]),
	%% invocation of the rpc handler
	#{RpcId := #{handler := MF, uri := Uri}} = Services,
	Res = sidejob:cast({PoolName, RequestId}, {MF, [Invocation, State]}),
	case Res of
		overload ->
			lager:info("Service overload"),
			awre:error(Con, RequestId, "overload", "worker pool exhausted", Uri),
			{noreply, State};
		_ ->
			{noreply, State}
	end.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
register_services(Con, Opts) ->
	Services = proplists:get_value(services, Opts),
	lists:foldl(fun ({Uri, MF}, Acc) -> 
		lager:info("register ~p ... ", [Uri]),
		{ok, RpcId} = awre:register(Con, [{invoke, roundrobin}], Uri),
		lager:info("registered (~p).", [RpcId]),
		Acc#{RpcId => #{uri => Uri, handler => MF}}
	end, #{}, Services).
