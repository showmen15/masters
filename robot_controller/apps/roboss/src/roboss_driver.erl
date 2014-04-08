-module(roboss_driver).
-behaviour(gen_server).

-include("../../include/records.hrl").
-include("include/roboss_pb.hrl").

-export([start_link/1, start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_wheels_cmd/2, request_state/1, request_robots_list/1, start_simulation/1, stop_simulation/1, reset_simulation/1]).

-define(CONNECTION_TIMEOUT, 1000).

-record(state, {
	port,
	robot_name
	}).

start_link(RobotName) -> 
	gen_server:start_link(?MODULE, RobotName, []).

start_link() -> 
	gen_server:start_link(?MODULE, undefined, []).

stop(Pid) -> 
	gen_server:call(Pid, stop).

send_wheels_cmd(Pid, WheelsCmd) ->
	gen_server:cast(Pid, {wheels_cmd, WheelsCmd}).

request_state(Pid) ->
	gen_server:call(Pid, request_state).

request_robots_list(Pid) ->
	gen_server:call(Pid, request_robots_list).

start_simulation(Pid) ->
	gen_server:cast(Pid, start_simulation).

stop_simulation(Pid) ->
	gen_server:cast(Pid, stop_simulation).

reset_simulation(Pid) ->
	gen_server:cast(Pid, reset_simulation).

%% gen_server callbacks
init(RobotName) ->
	{ok, Hostname} = application:get_env(hostname),
	{ok, TcpPort} = application:get_env(port),
	{ok, RobossInterfacePath} = application:get_env(roboss_interface_path),
	{ok, RobossInterfaceExe} = application:get_env(roboss_interface_exe),

	io:format("roboss_driver: ~s ~s ~s ~n", [Hostname, TcpPort, RobotName]),

	Args = case RobotName of
		undefined -> [Hostname, TcpPort];
		_ -> [Hostname, TcpPort, RobotName]
	end,

	process_flag(trap_exit, true),
	Port = open_port({spawn_executable, RobossInterfacePath ++ RobossInterfaceExe}, [
		{packet, 2}, 
		{args, Args},
		{cd, RobossInterfacePath}]),

	receive
		{Port, {data, Data}} ->
			#ack{} = roboss_pb:decode_ack(list_to_binary(Data)),
			send_pid_to_serv(RobotName),
			{ok, #state{port=Port, robot_name=RobotName}}
	after
		3000 ->
			{stop, connection_failed}
	end.	

handle_call(request_state, _From, State) when State#state.robot_name /= undefined ->
	%io:format("Got request state~n"),

	RequestPbRecord = #robossrequest{type = 'STATE_REQUEST'},

	EncodedRequest = roboss_pb:encode_robossrequest(RequestPbRecord),
	send_to_port(State, EncodedRequest),

	Port = State#state.port,
	receive
		{Port, {data, Data}} ->
			Reply = prepare_robot_state_reply(Data),
			{reply, Reply, State}

	after ?CONNECTION_TIMEOUT ->
		{reply, timeout, State}

	end;

handle_call(request_robots_list, _From, State) ->
	%io:format("Got request_robots_list ~n"),

	RequestPbRecord = #robossrequest{type = 'ROBOTS_LIST_REQUEST'},

	EncodedRequest = roboss_pb:encode_robossrequest(RequestPbRecord),
	send_to_port(State, EncodedRequest),

	Port = State#state.port,
	receive
		{Port, {data, Data}} ->
			Reply = prepare_robots_list_reply(Data),
			{reply, Reply, State}

	after 1000 ->
		{reply, timeout, State}

	end;

handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.


handle_cast({wheels_cmd, WheelsCmd}, State) when State#state.robot_name /= undefined ->
	%io:format("Got wheels_cmd ~w~n", [WheelsCmd]),
	
	WheelsCmdPbRecord = #wheelscommand{
		frontleft = WheelsCmd#wheels_cmd.front_left,
		frontright = WheelsCmd#wheels_cmd.front_right,
		rearleft = WheelsCmd#wheels_cmd.rear_left,
		rearright = WheelsCmd#wheels_cmd.rear_right
	},

	RequestPbRecord = #robossrequest{
		type = 'WHEELS_CMD',
		wheelscmd = WheelsCmdPbRecord
	},

	EncodedRequest = roboss_pb:encode_robossrequest(RequestPbRecord),
	send_to_port(State, EncodedRequest),

	{noreply, State};

handle_cast(start_simulation, State) ->
	io:format("Start simulation~n"),

	EncodedRequest = roboss_pb:encode_robossrequest(#robossrequest{type = 'START'}),
	send_to_port(State, EncodedRequest),

	{noreply, State};

handle_cast(stop_simulation, State) ->
	io:format("Stop simulation~n"),

	EncodedRequest = roboss_pb:encode_robossrequest(#robossrequest{type = 'STOP'}),
	send_to_port(State, EncodedRequest),

	{noreply, State};

handle_cast(reset_simulation, State) ->
	io:format("Reset simulation~n"),

	EncodedRequest = roboss_pb:encode_robossrequest(#robossrequest{type = 'RESET'}),
	send_to_port(State, EncodedRequest),

	{noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State};

handle_info(Info, State) ->
	io:format("info: ~w ~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

prepare_robot_state_reply(Data) ->
	RobotState = roboss_pb:decode_robotstate(list_to_binary(Data)),
	#robot_state{
		x = RobotState#robotstate.x,
		y = RobotState#robotstate.y,
		theta = RobotState#robotstate.theta,
		timestamp = RobotState#robotstate.timestamp
	}.

prepare_robots_list_reply(Data) ->
	RobotsList = roboss_pb:decode_robotslist(list_to_binary(Data)),
	RobotsList#robotslist.robotnames.

send_to_port(State, Msg) ->
	State#state.port ! {self(), {command, Msg}}.

send_pid_to_serv(undefined) ->
	ok;

send_pid_to_serv(RobotName) ->
	case roboss_serv:is_alive() of
		undefined -> undefined;
		_ -> roboss_serv:register_driver(RobotName, self()),
			ok 
	end.
