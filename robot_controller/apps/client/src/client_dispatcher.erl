-module(client_dispatcher).

-export([request_state/1, update_fear_factor/3, send_wheels_cmd/3]).

request_state(Pid) ->
	gen_server:call(Pid, request_state).

update_fear_factor(Pid, RobotName, FearFactor) ->
	gen_server:cast(Pid, {update_fear_factor, RobotName, FearFactor}).	

send_wheels_cmd(Pid, RobotName, WheelsCmd) ->
	gen_server:cast(Pid, {send_wheels_cmd, RobotName, WheelsCmd}).