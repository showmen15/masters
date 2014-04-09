-module(client_controller).
-behaviour(gen_server).

-include("include/client_pb.hrl").
-include("../../include/records.hrl").

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CONNECTION_TIMEOUT, 1000).

-record(state, {
	port,
	robot_name
	}).

start_link(RobotName) -> 
	net_kernel:connect_node(roboss@rose),
	gen_server:start_link(?MODULE, RobotName, []).

stop(Pid) -> 
	gen_server:call(Pid, stop).

%% gen_server callbacks
init(RobotName) ->
	%{ok, ClientPath} = application:get_env(	client, client_path),
	%{ok, ClientCommand} = application:get_env(client, client_command),
	ClientPath = "../python_controller/",
	ClientCommand = "controller.py",
	
	io:format("client_driver: ~s ~s ~s~n", [ClientPath, ClientCommand, RobotName]),

	process_flag(trap_exit, true),
	Port = open_port({spawn_executable, ClientPath ++ ClientCommand}, [
		{packet, 2}, 
		{args, [RobotName]},
		{cd, ClientPath}]),

	receive
		{Port, {data, Data}} ->
			#ack{} = client_pb:decode_ack(list_to_binary(Data)),
			State = #state{port=Port, robot_name=RobotName},
			{ok, State}
	after
		3000 ->
			{stop, connection_failed}
	end.	

handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({Port, {data, Data}}, State) when Port =:= State#state.port ->
	CmdMsg = #commandmessage{} = client_pb:decode_commandmessage(list_to_binary(Data)),
	case CmdMsg#commandmessage.type of
		'REQUEST_STATE' ->
			send_request_state_reply(State);

		'ROBOT_COMMAND' ->
			send_robot_command(State, CmdMsg#commandmessage.robotcommand);

		_ ->
			io:format("Unknown CommandMessage type~n")
	end,
	{noreply, State};

handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_terminated, Reason}, State};

handle_info(Info, State) ->
	io:format("info: ~w ~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

send_to_port(State, Msg) ->
	State#state.port ! {self(), {command, Msg}}.

send_request_state_reply(State) ->
	{ok, RobotsStateDict} = client_state_manager:request_state(),

	Fun = fun ({RobotName, RobotState}) ->
		#robotfullstate{
			robotname = RobotName,
			x = RobotState#robot_state.x,
			y = RobotState#robot_state.y,
			theta = RobotState#robot_state.theta,
			timestamp = RobotState#robot_state.timestamp
		} end,

	StatesList = lists:map(Fun, dict:to_list(RobotsStateDict)),

	StateMsg = #statemessage{robotstate = StatesList},
	StateMsgEncoded = client_pb:encode_statemessage(StateMsg),
	send_to_port(State, StateMsgEncoded).

send_robot_command(State, RobotCommand) ->
	WheelsCmd = prepare_wheels_cmd(RobotCommand),
	roboss_serv:send_wheels_cmd(State#state.robot_name, WheelsCmd).

prepare_wheels_cmd(RobotCommand) ->
	#wheels_cmd{
		front_left = RobotCommand#robotcommand.frontleft,
		front_right = RobotCommand#robotcommand.frontright,
		rear_left = RobotCommand#robotcommand.rearleft,
		rear_right = RobotCommand#robotcommand.rearright
	}.