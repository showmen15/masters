-module(roboss_notifiers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2, terminate_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(RobotName, Pid) ->
	io:format("~s: child starting~n", [?MODULE]),
	supervisor:start_child(?MODULE, [RobotName, Pid]).

terminate_child(Pid) ->
	io:format("~s: child terminating~n", [?MODULE]),
	supervisor:terminate_child(?MODULE, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
	MaxRestart = 1,
	MaxTime = 10,
	ChildSpec = {
		roboss_notifier,
		{roboss_notifier, start_link, []},
		permanent,
		1000,
		worker,
		[roboss_notifier]
	},

    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.