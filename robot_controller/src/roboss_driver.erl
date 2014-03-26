-module(roboss_driver).
-behaviour(gen_server).

-include("include/records.hrl").

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
	gen_server:cast(start_simulation).

stop_simulation(Pid) ->
	gen_server:cast(stop_simulation).

reset_simulation(Pid) ->
	gen_server:cast(reset_simulation).

%% gen_server callbacks
init({Hostname, TcpPort, RobotName}) ->
	io:format("Init ~s ~s ~s ~n", [Hostname, TcpPort, RobotName]),

	PortArgs = string:join([Hostname, TcpPort, RobotName], " "),
	Port = open_port({spawn_executable, ?ROBOSS_INTERFACE_PATH ++ ?ROBOSS_INTERFACE_EXE}, [
		{packet, 2}, 
		{args, [Hostname, TcpPort, RobotName]},
		{cd, ?ROBOSS_INTERFACE_PATH}]),

	{ok, #state{port=Port, robot_name=RobotName}}.

handle_call(request_state, From, State) ->
	io:format("Got request state~n");


handle_call(terminate, _From, State) ->
	{stop, normal, ok, State}.


handle_cast({wheels_cmd, WheelsCmd}, State) ->
	io:format("Got wheels_cmd ~w~n", [WheelsCmd]),
	{noreply, State};

handle_cast(Request, State) ->
	ok.


handle_info(Info, State) ->
	ok.

terminate(Reason, State) ->
	ok.

code_change(OldVsn, State, Extra) ->
	ok.