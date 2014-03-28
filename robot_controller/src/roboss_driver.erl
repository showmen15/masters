-module(roboss_driver).
-behaviour(gen_server).

-include("include/records.hrl").
-include("include/roboss_pb.hrl").

-export([start_link/3, exit/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_wheels_cmd/2, request_state/1, start_simulation/1, stop_simulation/1, reset_simulation/1]).

-define(ROBOSS_INTERFACE_PATH, "../roboss_interface/bin/Debug/").
-define(ROBOSS_INTERFACE_EXE, "RobossInterface.exe").

-record(state, {
	port,
	robot_name
	}).

start_link(Hostname, Port, RobotName) -> 
	gen_server:start_link(?MODULE, {Hostname, Port, RobotName}, []).

exit(Pid) -> 
	gen_server:call(Pid, terminate).

send_wheels_cmd(Pid, WheelsCmd) ->
	gen_server:cast(Pid, {wheels_cmd, WheelsCmd}).

request_state(Pid) ->
	gen_server:call(Pid, request_state).

start_simulation(Pid) ->
	gen_server:cast(Pid, start_simulation).

stop_simulation(Pid) ->
	gen_server:cast(Pid, stop_simulation).

reset_simulation(Pid) ->
	gen_server:cast(Pid, reset_simulation).

%% gen_server callbacks
init({Hostname, TcpPort, RobotName}) ->
	io:format("Init ~s ~s ~s ~n", [Hostname, TcpPort, RobotName]),

	%process_flag(trap_exit, true),
	Port = open_port({spawn_executable, ?ROBOSS_INTERFACE_PATH ++ ?ROBOSS_INTERFACE_EXE}, [
		{packet, 2}, 
		{args, [Hostname, TcpPort, RobotName]},
		{cd, ?ROBOSS_INTERFACE_PATH},
		exit_status]),

	{ok, #state{port=Port, robot_name=RobotName}}.

handle_call(request_state, _From, State) ->
	io:format("Got request state~n"),

	RequestPbRecord = #robossrequest{type = 'STATE_REQUEST'},

	EncodedRequest = roboss_pb:encode_robossrequest(RequestPbRecord),
	send_to_port(State, EncodedRequest),

	Port = State#state.port,
	receive
		{Port, {data, Data}} ->
			Reply = prepare_robot_state_reply(Data),
			{reply, Reply, State}

	after 1000 ->
		{reply, timeout, State}

	end;

handle_call(terminate, _From, State) ->
	{stop, normal, ok, State}.


handle_cast({wheels_cmd, WheelsCmd}, State) ->
	io:format("Got wheels_cmd ~w~n", [WheelsCmd]),
	
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
	io:format("~w ~n", [Info]),
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

send_to_port(State, Msg) ->
	State#state.port ! {self(), {command, Msg}}.
