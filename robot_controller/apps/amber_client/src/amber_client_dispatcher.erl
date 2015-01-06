-module(amber_client_dispatcher).

-behaviour(gen_server).

-export([update_robot_states/0, start_states_updater/0, states_updater/1]).
-export([start_location_updater/0, location_updater/1]).
-export([start_link/0, stop/0, update_my_location/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/records.hrl").
-include("../state_manager/include/records.hrl").
-include("include/location_pb.hrl").


-record(state, {
	robots_dict = dict:new(),
	ff_dict = dict:new(),
	robot_name,
	my_ff = 0.0,
	my_location
	}).

%% Public API

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

update_my_location(MyLocation) ->
	gen_server:cast(?MODULE, {update_my_location, MyLocation}).

update_robot_states() ->
	gen_server:cast(?MODULE, update_robot_states).

start_location_updater() ->
	{ok, UpdatePeriod} = application:get_env(amber_client, location_refresh_period),
	Pid = spawn_link(?MODULE, location_updater, [UpdatePeriod]),
	{ok, Pid}.

location_updater(UpdatePeriod) ->
	receive
	after UpdatePeriod ->
		MyLocation = amber_client_location:request_location(),
		amber_client_dispatcher:update_my_location(MyLocation)
	end,
	location_updater(UpdatePeriod).


start_states_updater() ->
	{ok, UpdatePeriod} = application:get_env(amber_client, states_refresh_period),
	Pid = spawn_link(?MODULE, states_updater, [UpdatePeriod]),
	{ok, Pid}.

states_updater(UpdatePeriod) ->
	receive
	after UpdatePeriod ->
		amber_client_dispatcher:update_robot_states()
	end,
	states_updater(UpdatePeriod).

%% Callbacks

init(_Args) ->
	io:format("~s started~n", [?MODULE]),
	self() ! spawn_client,
	{ok, [[RobotName]]} = init:get_argument(robot_name),
	{ok, #state{robot_name = RobotName}}.

handle_call(request_state, _From, #state{robots_dict = Dict, ff_dict = FFDict} = State) ->
	{reply, {ok, Dict, FFDict}, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({update_fear_factor, _RobotName, FearFactor}, State) ->
	{noreply, State#state{my_ff = FearFactor}};

handle_cast({send_wheels_cmd, _RobotName, WheelsCmd}, State) ->
	amber_client_roboclaw:send_wheels_cmd(WheelsCmd),
	{noreply, State};

handle_cast(update_robot_states, State) ->
	RobotStates = state_manager:get_states(),

	Fun = fun (_RobotName, MS) ->
		#robot_state{
			x = MS#robot_manager_state.x,
			y = MS#robot_manager_state.x,
			theta = MS#robot_manager_state.theta,
			timestamp = MS#robot_manager_state.timestamp
		}
	end,
	RobotsDict = dict:map(Fun, RobotStates),

	FfFun = fun (_RobotName, MS) ->
		MS#robot_manager_state.ff
	end,
	FFDict = dict:map(FfFun, RobotStates),

	{noreply, State#state{robots_dict = RobotsDict, ff_dict = FFDict}};

handle_cast({update_my_location, MyLocation}, 
	#state{robot_name = RobotName, my_ff = FF} = State) ->
	
	%io:format("~w ~w My location updated~n", [RobotName, MyLocation]),

	state_manager:update(RobotName, {
		MyLocation#location.x,
		MyLocation#location.y,
		MyLocation#location.alfa,
		FF,
		MyLocation#location.timestamp
		}),

	{noreply, State#state{my_location = MyLocation}};
 
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(spawn_client, State) ->
	client_controllers_sup:spawn_client(State#state.robot_name, self()),
	{noreply, State};

handle_info({notification, RobotName, RobotState}, #state{robots_dict = Dict, ff_dict = FFDict} = State) ->

	{DictToStore, NewFFDict} = case dict:find(RobotName, Dict) of
		{ok, OldState} ->
			OldTimestamp = OldState#robot_state.timestamp,
			NewTimestamp = RobotState#robot_state.timestamp,

			if
				NewTimestamp < OldTimestamp ->
					io:format("state_manager: reset~n"),
					client_controllers_sup:set_event(reset),
					{dict:new(), dict:new()};
				true ->
					{Dict, FFDict}
			end;

		error ->
			{Dict, FFDict}
	end,

	NewDict = dict:store(RobotName, RobotState, DictToStore),
	{noreply, State#state{robots_dict = NewDict, ff_dict = NewFFDict}};

handle_info(Info, State) ->
	io:format("info: ~w ~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	