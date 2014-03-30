-module(roboss_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init() ->
	MaxRestart = 1,
	MaxTime = 10,
	ChildSpec = {
		serv,
		{roboss_serv, start_link, []},
		permanent,
		1000,
		worker,
		[roboss_serv]
	},

    {ok, {{one_for_all, MaxRestart, MaxTime}, [ChildSpec]}}.