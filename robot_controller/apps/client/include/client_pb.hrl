-ifndef(STATEMESSAGE_PB_H).
-define(STATEMESSAGE_PB_H, true).
-record(statemessage, {
    robotstate = [],
    event
}).
-endif.

-ifndef(ROBOTFULLSTATE_PB_H).
-define(ROBOTFULLSTATE_PB_H, true).
-record(robotfullstate, {
    robotname = erlang:error({required, robotname}),
    x = erlang:error({required, x}),
    y = erlang:error({required, y}),
    theta = erlang:error({required, theta}),
    timestamp = erlang:error({required, timestamp})
}).
-endif.

-ifndef(SETUPMESSAGE_PB_H).
-define(SETUPMESSAGE_PB_H, true).
-record(setupmessage, {
    robotname = erlang:error({required, robotname})
}).
-endif.

-ifndef(ACK_PB_H).
-define(ACK_PB_H, true).
-record(ack, {
    
}).
-endif.

-ifndef(COMMANDMESSAGE_PB_H).
-define(COMMANDMESSAGE_PB_H, true).
-record(commandmessage, {
    type = erlang:error({required, type}),
    robotcommand
}).
-endif.

-ifndef(ROBOTCOMMAND_PB_H).
-define(ROBOTCOMMAND_PB_H, true).
-record(robotcommand, {
    frontleft = erlang:error({required, frontleft}),
    frontright = erlang:error({required, frontright}),
    rearleft = erlang:error({required, rearleft}),
    rearright = erlang:error({required, rearright})
}).
-endif.

