-module(client_state_manager).

-behaviour(gen_server).

-export([start_link/0, stop/0, request_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/records.hrl").

-record(state, {
	robots_dict = dict:new()
	}).

%% Public API

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

request_state() ->
	gen_server:call(?MODULE, request_state).

%% Callbacks

init(_Args) ->
	io:format("~s started~n", [?MODULE]),
	roboss_serv:request_notifies(self()),
	{ok, #state{}}.

handle_call(request_state, _From, #state{robots_dict = Dict} = State) ->
	{reply, {ok, Dict}, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({notification, RobotName, RobotState}, #state{robots_dict = Dict} = State) ->
	NewDict = dict:store(RobotName, RobotState, Dict),
	{noreply, State#state{robots_dict = NewDict}};

handle_info(Info, State) ->
	io:format("info: ~w ~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	