-ifndef(DRIVERHDR_PB_H).
-define(DRIVERHDR_PB_H, true).
-record(driverhdr, {
    devicetype,
    deviceid,
    clientids = []
}).
-endif.

-ifndef(DRIVERMSG_PB_H).
-define(DRIVERMSG_PB_H, true).
-record(drivermsg, {
    type = erlang:error({required, type}),
    synnum,
    acknum,
    listenernum,
    '$extensions' = dict:new()
}).
-endif.

