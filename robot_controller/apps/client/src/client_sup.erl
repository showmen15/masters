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
		controllers_sup,
		{client_controllers_sup, start_link, []},
		permanent,
		1000,
		worker,
		[client_clients_sup]
	},

	DispatcherSpec = {
		dispatcher,
		{roboss_client_dispatcher, start_link, []},
		permanent,
		1000,
		worker,
		[roboss_client_dispatcher]
	},

	ServSpec = {
		serv,
		{client_serv, start_link, []},
		permanent,
		1000,
		worker,
		[client_serv]
	},

	UdpServSpec = {
		udp_serv,
		{client_udp_serv, start_link, []},
		permanent,
		1000,
		worker,
		[client_udp_serv]
	},

    {ok, {{one_for_all, MaxRestart, MaxTime}, [SupSpec, DispatcherSpec, ServSpec, UdpServSpec]}}.

