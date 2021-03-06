-module(client_controller).
-behaviour(gen_server).

-include("include/client_pb.hrl").
-include("../../include/records.hrl").

-export([start_link/2, stop/1, set_event/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CONNECTION_TIMEOUT, 1000).

-record(state, {
	port,
	robot_name,
	event = undef,
	dispatcher_pid
	}).

start_link(RobotName, DispatcherPid) -> 
	gen_server:start_link(?MODULE, {RobotName, DispatcherPid}, []).

stop(Pid) -> 
	gen_server:call(Pid, stop).

set_event(Pid, Event) ->
	gen_server:cast(Pid, {set_event, Event}).

%% gen_server callbacks
init({RobotName, DispatcherPid}) ->
	{ok, ClientPath} = application:get_env(client, client_path),
	{ok, ClientCommand} = application:get_env(client, client_command),
	{ok, RosonDir} = application:get_env(client, roson_dir),

	Args = case init:get_argument(world) of
		{ok, World} -> 
			RosonArg = RosonDir ++ World ++ ".roson",
			[RobotName, RosonArg];
		error ->
			[RobotName]
		end,

	io:format("client_driver: ~s ~s ~s~n", [ClientPath, ClientCommand, string:join(Args, " ")]),

	process_flag(trap_exit, true),
	Port = open_port({spawn_executable, ClientPath ++ ClientCommand}, [
		{packet, 2}, 
		{args, Args},
		{cd, ClientPath}]),

	receive
		{Port, {data, Data}} ->
			#ack{} = client_pb:decode_ack(list_to_binary(Data)),
			State = #state{port=Port, robot_name=RobotName, dispatcher_pid=DispatcherPid},
			{ok, State}
	after
		3000 ->
			{stop, connection_failed}
	end.	


handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.


handle_cast({set_event, start}, State) ->
	{noreply, State#state{event = 'START'}};

handle_cast({set_event, stop}, State) ->
	{noreply, State#state{event = 'STOP'}};

handle_cast({set_event, reset}, State) ->
	{noreply, State#state{event = 'RESET'}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({Port, {data, Data}}, State) when Port =:= State#state.port ->
	CmdMsg = #commandmessage{} = client_pb:decode_commandmessage(list_to_binary(Data)),
	NewState = case CmdMsg#commandmessage.type of
		'REQUEST_STATE' ->
			send_request_state_reply(State),
			State#state{event = undef};

		'ROBOT_COMMAND' ->
			RobotCommand = CmdMsg#commandmessage.robotcommand,
			send_robot_command(State, RobotCommand),
			client_dispatcher:update_fear_factor(
				State#state.dispatcher_pid, 
				State#state.robot_name, 
				RobotCommand#robotcommand.fearfactor),
			State;

		_ ->
			io:format("Unknown CommandMessage type~n"),
			State
	end,
	{noreply, NewState};

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
	{ok, RobotsStateDict, FFDict} = client_dispatcher:request_state(State#state.dispatcher_pid),

	Fun = fun ({RobotName, RobotState}) ->
		FearFactor = case dict:find(RobotName, FFDict) of
			{ok, FF} ->
				FF;
			error -> 
				0.0
		end,

		#robotfullstate{
			robotname = RobotName,
			x = RobotState#robot_state.x,
			y = RobotState#robot_state.y,
			theta = RobotState#robot_state.theta,
			timestamp = RobotState#robot_state.timestamp,
			fearfactor = FearFactor
		} end,

	StatesList = lists:map(Fun, dict:to_list(RobotsStateDict)),

	StateMsg = case State#state.event of
		undef -> 
			#statemessage{robotstate = StatesList};

		Event -> 
			#statemessage{robotstate = StatesList, event = Event}
	end,	

	StateMsgEncoded = client_pb:encode_statemessage(StateMsg),
	send_to_port(State, StateMsgEncoded).

send_robot_command(#state{dispatcher_pid = DispatcherPid, robot_name = RobotName} = State,
		RobotCommand) ->
	WheelsCmd = prepare_wheels_cmd(RobotCommand),
	client_dispatcher:send_wheels_cmd(DispatcherPid, RobotName, WheelsCmd).

prepare_wheels_cmd(RobotCommand) ->
	#wheels_cmd{
		front_left = RobotCommand#robotcommand.frontleft,
		front_right = RobotCommand#robotcommand.frontright,
		rear_left = RobotCommand#robotcommand.rearleft,
		rear_right = RobotCommand#robotcommand.rearright
	}.