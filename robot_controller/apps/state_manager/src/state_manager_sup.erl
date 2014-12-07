-module(state_manager_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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

init([]) ->
    MaxRestart = 1,
	MaxTime = 10,
	StateManagerSpec = {
		state_manager,
		{state_manager, start_link, []},
		permanent,
		1000,
		worker,
		[state_manager]
	},

	EvicterSpec = {
		evicter,
		{state_manager, start_evicter, []},
		permanent,
		1000,
		worker,
		[evicter]
	},

    {ok, {{one_for_all, MaxRestart, MaxTime}, [StateManagerSpec, EvicterSpec]}}.

