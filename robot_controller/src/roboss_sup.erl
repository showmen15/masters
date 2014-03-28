-module(roboss_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(StartArgs) ->
	MaxRestart = 1,
	MaxTime = 10,
	ChildSpec = {
		serv,
		{roboss_serv, start_link, [StartArgs]},
		permanent,
		1000,
		worker,
		[roboss_serv]
	},

    {ok, {{one_for_all, MaxRestart, MaxTime}, [ChildSpec]}}.

