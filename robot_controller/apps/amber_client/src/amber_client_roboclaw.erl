-module(amber_client_roboclaw).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, code_change/3, handle_call/3, terminate/2, handle_info/2]).
-export([send_wheels_cmd/1]).

-include("../../include/records.hrl").
-include("include/drivermsg_pb.hrl").
-include("include/roboclaw_pb.hrl").

-record(state, {
	address,
	port,
	socket
	}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_wheels_cmd(WheelsCmd) ->
	gen_server:cast(?MODULE, {send_wheels_cmd, WheelsCmd}).

init(_Args) ->
	{ok, AmberAddress} = application:get_env(amber_client, amber_address),
	{ok, AmberPort} = application:get_env(amber_client, amber_port),
	{ok, Socket} = gen_udp:open(0, [binary, {active, false}]),

	io:format("~s started~n", [?MODULE]),
	{ok, #state{address = AmberAddress, port = AmberPort, socket = Socket}}.

handle_cast({send_wheels_cmd, WheelsCmd}, State) ->
	MotorsSpeed = #motorsspeed{
		frontleftspeed = round(WheelsCmd#wheels_cmd.front_left * 1000),
		frontrightspeed = round(WheelsCmd#wheels_cmd.front_right * 1000),
		rearleftspeed = round(WheelsCmd#wheels_cmd.rear_left * 1000),
		rearrightspeed = round(WheelsCmd#wheels_cmd.rear_right * 1000)
	},

	{ok, Msg} = roboclaw_pb:set_extension(#drivermsg{
		type = 'DATA',
		synnum = 100},
		motorscommand, MotorsSpeed),

	#state{socket = Socket, port = Port, address = Address} = State,

	MsgList = roboclaw_pb:encode_drivermsg(Msg),
	HdrList = amber_client_msg:build_header(2, 0),

	amber_client_msg:send_message(HdrList, MsgList, Socket, Address, Port), 

	{noreply, State}.
	

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

handle_info(_Info, State) ->
    {noreply, State}.
