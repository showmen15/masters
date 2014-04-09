-module(roboss_serv).

-behaviour(gen_server).
-export([start_link/0, stop/0, send_wheels_cmd/2, request_state/1, register_driver/2, is_alive/0, get_robots_list/0]).
-export([request_notifies/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/records.hrl").

-record(state, {
	robots_dict = dict:new()
	}).

%% Public API

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call({global, ?MODULE}, stop).

send_wheels_cmd(RobotName, WheelsCmd) ->
	gen_server:call({global, ?MODULE}, {send_wheels_cmd, {RobotName, WheelsCmd}}).

request_state(RobotName) ->
	gen_server:call({global, ?MODULE}, {request_state, {RobotName}}).

register_driver(RobotName, Pid) ->
	gen_server:cast({global, ?MODULE}, {register_driver, {RobotName, Pid}}).

request_notifies(Pid) ->
	gen_server:call({global, ?MODULE}, {request_notifies, Pid}).

is_alive() ->
	case global:whereis_name(?MODULE) of
		undefined -> false;
		_ -> true
	end.

get_robots_list() ->
	gen_server:call({global, ?MODULE}, get_robots_list).

%% Callbacks

init(_Args) ->
	io:format("roboss_serv~n"),
	
	State = #state{},
	self() ! start_control_driver,
	
	{ok, State}.


handle_call({send_wheels_cmd, {RobotName, WheelsCmd}}, _From, State) ->
	RobotsDict = State#state.robots_dict,
	Pid = dict:fetch(RobotName, RobotsDict),
	roboss_driver:send_wheels_cmd(Pid, WheelsCmd),
	{reply, ok, State};

handle_call({request_state, {RobotName}}, _From, State) ->
	%io:format("rs~n"),
	RobotsDict = State#state.robots_dict,
	Pid = dict:fetch(RobotName, RobotsDict),
	Reply = roboss_driver:request_state(Pid),
	{reply, {ok, Reply}, State};

handle_call(get_robots_list, _From, State) ->
	Reply = dict:fetch_keys(State#state.robots_dict),
	{reply, {ok, Reply}, State};

handle_call({request_notifies, Pid}, _From, State) ->
	_PidsList = spawn_notifiers(State, Pid),
	{reply, ok, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({register_driver, {RobotName, Pid}}, #state{robots_dict = RobotsDict} = State) ->
	io:format("register_driver ~s ~w~n", [RobotName, Pid]),
	NewRobotsDict = dict:store(RobotName, Pid, RobotsDict),

	{noreply, State#state{robots_dict = NewRobotsDict}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(start_control_driver, State) ->
	{ok, Pid} = spawn_control_driver(State),
	RobotsList = roboss_driver:request_robots_list(Pid),
	spawn_robots_drivers(RobotsList),
	{noreply, State};

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
		temporary,
		1000,
		worker,
		[roboss_driver]
	},
	supervisor:start_child(roboss_sup, ChildSpec).

spawn_robots_drivers(RobotsList) ->
	lists:map(
		fun (RobotName) -> spawn_robot_driver(RobotName) end,
		RobotsList).

spawn_robot_driver(RobotName) ->
	{ok, _Pid} = supervisor:start_child(roboss_drivers_sup, [RobotName]).

spawn_notifiers(State, ListenerPid) ->
	RobotsList = dict:fetch_keys(State#state.robots_dict),
	NotifiersPids = lists:map(
		fun (RobotName) -> 
			{ok, NotifierPid} = roboss_notifiers_sup:start_child(RobotName, ListenerPid),
			NotifierPid
		end,
		RobotsList),

	roboss_pingers_sup:start_child(ListenerPid, NotifiersPids).


