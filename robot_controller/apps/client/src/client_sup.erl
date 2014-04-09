-module(client_sup).

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
		clients_sup,
		{client_clients_sup, start_link, []},
		permanent,
		1000,
		worker,
		[client_clients_sup]
	},

	StateManagerSpec = {
		state_manager,
		{client_state_manager, start_link, []},
		permanent,
		1000,
		worker,
		[client_state_manager]
	},

	ServSpec = {
		serv,
		{client_serv, start_link, []},
		permanent,
		1000,
		worker,
		[client_serv]
	},

    {ok, {{one_for_all, MaxRestart, MaxTime}, [SupSpec, StateManagerSpec, ServSpec]}}.

