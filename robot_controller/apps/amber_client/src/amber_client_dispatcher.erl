-module(amber_client_dispatcher).

-behaviour(gen_server).

-export([start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/records.hrl").

-record(state, {
	robots_dict = dict:new(),
	ff_dict = dict:new(),
	robot_name
	}).

%% Public API

start_link() ->
	gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_server:call(Pid, stop).

%% Callbacks

init(_Args) ->
	io:format("~s started~n", [?MODULE]),
	self() ! spawn_client,
	{ok, RobotName} = init:get_argument(robot_name),
	{ok, #state{robot_name = RobotName}}.

handle_call(request_state, _From, #state{robots_dict = Dict, ff_dict = FFDict} = State) ->
	{reply, {ok, Dict, FFDict}, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({update_fear_factor, RobotName, FearFactor}, #state{ff_dict = FFDict} = State) ->
	NewFFDict = dict:store(RobotName, FearFactor, FFDict),
	{noreply, State#state{ff_dict = NewFFDict}};

handle_cast({send_wheels_cmd, _RobotName, WheelsCmd}, State) ->
	amber_client_roboclaw:send_wheels_cmd(WheelsCmd),
	{noreply, State};
 
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
	