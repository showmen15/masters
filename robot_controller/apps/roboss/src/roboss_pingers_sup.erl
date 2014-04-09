-module(roboss_pingers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ListenerPid, NotifiersPids) ->
	supervisor:start_child(?MODULE, [ListenerPid, NotifiersPids]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
	MaxRestart = 1,
	MaxTime = 10,
	ChildSpec = {
		roboss_pinger,
		{roboss_pinger, start_link, []},
		temporary,
		1000,
		worker,
		[roboss_pinger]
	},

    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.