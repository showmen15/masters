-module(roboss_notifier).

-export([start_link/2, init/2]).

start_link(RobotName, Pid) ->
	{ok, spawn_link(?MODULE, init, [RobotName, Pid])}.

init(RobotName, Pid) ->
	io:format("~s started: ~s~n", [?MODULE, RobotName]),
	loop(RobotName, Pid).

loop(RobotName, Pid) ->
	receive
		after 1000 -> 
			notify(RobotName, Pid),
			loop(RobotName, Pid)
	end.

notify(RobotName, Pid) ->
	{ok, RobotState} = roboss_serv:request_state(RobotName),
	Pid ! {notification, RobotName, RobotState}.