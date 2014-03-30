-module(roboss_serv).

-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/records.hrl"),

-record(state, {
	robots_dict
	}).

%% Public API

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

send_wheels_cmd(RobotName, WheelsCmd) ->
	ok.

request_state(RobotName, WheelsCmd) ->
	ok.

%% Callbacks

init(_Args) ->
	io:format("roboss_serv~n"),
	State = #state{},

	self() ! start_control_driver,
	
	{ok, State}.


handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};


handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(start_control_driver, State) ->
	{ok, Pid} = spawn_control_driver(State),
	RobotsList = roboss_driver:request_robots_list(Pid),
	
	NewState = spawn_robots_drivers(RobotsList, State),

	{noreply, NewState};


handle_info(Info, State) ->
	io:format("info: ~w ~n", [Info]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

spawn_control_driver(_State) ->
	ChildSpec = {
		control_driver,
		{roboss_driver, start_link, []},
		permanent,
		1000,
		worker,
		[roboss_driver]
	},
	{ok, _Pid} = supervisor:start_child(roboss_sup, ChildSpec).



spawn_robots_drivers(RobotsList, State) ->
	RobotsDict = lists:foldl(
		fun (RobotName, Dict) -> spawn_robot_driver(RobotName, Dict) end,
		dict:new(),
		RobotsList),


	State#state{robots_dict = RobotsDict}.


spawn_robot_driver(RobotName, RobotsDict) ->
	ChildSpec = {
		RobotName,
		{roboss_driver, start_link, [RobotName]},
		permanent,
		1000,
		worker,
		[roboss_driver]
	},
	{ok, Pid} = supervisor:start_child(RobotName, ChildSpec),

	dict:append(RobotName, Pid, RobotsDict).