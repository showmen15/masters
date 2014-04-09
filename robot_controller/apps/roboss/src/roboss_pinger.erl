-module(roboss_pinger).

-export([start_link/2, init/2]).

start_link(ListenerPid, NotifiersPids) ->
	{ok, spawn_link(?MODULE, init, [ListenerPid, NotifiersPids])}.

init(ListenerPid, NotifiersPids) ->
	loop(ListenerPid, NotifiersPids).

loop(ListenerPid, NotifiersPids) ->
	receive
		after 1000 -> 
			case rpc:pinfo(ListenerPid) of
				undefined ->
					io:format("listener dead~n", []),
					lists:map(
						fun(Pid) -> roboss_notifiers_sup:terminate_child(Pid) end,
						NotifiersPids);
				_ ->
					loop(ListenerPid, NotifiersPids)	
			end
	end.
