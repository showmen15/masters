-module(client_serv).

-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../../include/records.hrl").

-record(state, {
	}).

%% Public API

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

%% Callbacks

init(_Args) ->
	io:format("~s started~n", [?MODULE]),
	self() ! spawn_clients,
	{ok, #state{}}.

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(spawn_clients, State) ->
	{ok, RobotsList} = roboss_serv:get_robots_list(),
	lists:map(
		fun (RobotName) -> client_controllers_sup:spawn_client(RobotName) end,
		RobotsList),
	{noreply, State};

handle_info(Info, State) ->
	io:format("info: ~w ~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	