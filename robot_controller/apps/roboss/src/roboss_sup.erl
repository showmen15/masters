-module(roboss_sup).

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
	
	DriverSupSpec = {
		drivers_sup,
		{roboss_drivers_sup, start_link, []},
		temporary,
		1000,
		supervisor,
		[roboss_drivers_sup]
	},

	NotifiersSupSpec = {
		notifiers_sup,
		{roboss_notifiers_sup, start_link, []},
		temporary,
		1000,
		supervisor,
		[roboss_notifiers_sup]
	},

	ServSpec = {
		serv,
		{roboss_serv, start_link, []},
		permanent,
		1000,
		worker,
		[roboss_serv]
	},



    {ok, {{one_for_all, MaxRestart, MaxTime}, [DriverSupSpec, NotifiersSupSpec, ServSpec]}}.