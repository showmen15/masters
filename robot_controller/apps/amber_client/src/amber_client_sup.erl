-module(amber_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
	MaxRestart = 1,
	MaxTime = 10,
	SupSpec = {
		client_controllers_sup,
		{client_controllers_sup, start_link, []},
		permanent,
		1000,
		worker,
		[client_controllers_sup]
	},

	DispatcherSpec = {
		dispatcher,
		{amber_client_dispatcher, start_link, []},
		permanent,
		1000,
		worker,
		[amber_client_dispatcher]
	},

	RoboclawClientSpec = {
		roboclaw_client,
		{amber_client_roboclaw, start_link, []},
		permanent,
		1000,
		worker,
		[amber_client_roboclaw]
	},

	LocationClientSpec = {
		location_client,
		{amber_client_location, start_link, []},
		permanent,
		1000,
		worker,
		[amber_client_location]
	},

	LocationUpdaterSpec = {
		location_updater,
		{amber_client_dispatcher, start_location_updater, []},
		permanent,
		1000,
		worker,
		[location_updater]
	},

	StatesUpdaterSpec = {
		states_updater,
		{amber_client_dispatcher, start_states_updater, []},
		permanent,
		1000,
		worker,
		[states_updater]
	},


    {ok, {{one_for_all, MaxRestart, MaxTime}, 
    	[SupSpec, DispatcherSpec, RoboclawClientSpec, 
    	LocationClientSpec, LocationUpdaterSpec, StatesUpdaterSpec]}}.

