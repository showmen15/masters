-module(client_clients_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, spawn_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_client(RobotName) ->
	supervisor:start_child(?MODULE, [RobotName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
	MaxRestart = 1,
	MaxTime = 10,
	ChildSpec = {
		client_controller,
		{client_controller, start_link, []},
		permanent,
		1000,
		worker,
		[client_controller]
	},

    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.