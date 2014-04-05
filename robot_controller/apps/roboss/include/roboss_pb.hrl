-ifndef(ROBOTSTATE_PB_H).
-define(ROBOTSTATE_PB_H, true).
-record(robotstate, {
    x = erlang:error({required, x}),
    y = erlang:error({required, y}),
    theta = erlang:error({required, theta}),
    timestamp = erlang:error({required, timestamp})
}).
-endif.

-ifndef(ROBOSSREQUEST_PB_H).
-define(ROBOSSREQUEST_PB_H, true).
-record(robossrequest, {
    type = erlang:error({required, type}),
    wheelscmd
}).
-endif.

-ifndef(WHEELSCOMMAND_PB_H).
-define(WHEELSCOMMAND_PB_H, true).
-record(wheelscommand, {
    frontleft = erlang:error({required, frontleft}),
    frontright = erlang:error({required, frontright}),
    rearleft = erlang:error({required, rearleft}),
    rearright = erlang:error({required, rearright})
}).
-endif.

-ifndef(ROBOTSLIST_PB_H).
-define(ROBOTSLIST_PB_H, true).
-record(robotslist, {
    robotnames = []
}).
-endif.

-ifndef(ACK_PB_H).
-define(ACK_PB_H, true).
-record(ack, {
    
}).
-endif.

