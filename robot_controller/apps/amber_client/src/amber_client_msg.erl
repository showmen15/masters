-module(amber_client_msg).

-include("include/drivermsg_pb.hrl").
-include("include/roboclaw_pb.hrl").

-export([build_header/2, send_message/5, pack_msg/2]).

build_header(DeviceType, DeviceID) ->
	Hdr = #driverhdr{devicetype = DeviceType, deviceid = DeviceID},
	drivermsg_pb:encode_driverhdr(Hdr).

send_message(Hdr, Msg, Socket, Hostname, Port) ->
	gen_udp:send(Socket, Hostname, Port, pack_msg(Hdr, Msg)).

pack_msg(Hdr, Msg) ->
	HdrBinary = erlang:iolist_to_binary(Hdr),
	MsgBinary = erlang:iolist_to_binary(Msg),
	HdrLen = byte_size(HdrBinary),
	MsgLen = byte_size(MsgBinary),
	<<HdrLen:2/big-unsigned-integer-unit:8, HdrBinary/binary,
		MsgLen:2/big-unsigned-integer-unit:8, MsgBinary/binary>>.