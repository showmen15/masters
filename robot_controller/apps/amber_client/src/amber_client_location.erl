-module(amber_client_location).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, code_change/3, handle_call/3, terminate/2, handle_info/2]).
-export([request_location/0]).

-include("../../include/records.hrl").
-include("include/drivermsg_pb.hrl").
-include("include/location_pb.hrl").

-record(state, {
	address,
	port,
	socket,
	synnum = 0
	}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

request_location() ->
	gen_server:call(?MODULE, request_location).

init(_Args) ->
	{ok, AmberAddress} = application:get_env(amber_client, amber_address),
	{ok, AmberPort} = application:get_env(amber_client, amber_port),
	{ok, Socket} = gen_udp:open(0, [binary, {active, false}]),

	io:format("~s started~n", [?MODULE]),
	{ok, #state{address = AmberAddress, port = AmberPort, socket = Socket}}.

handle_call(request_location, _From, State) ->
	
	#state{socket = Socket, port = Port, address = Address, synnum = SynNum} = State,

	{ok, Msg} = location_pb:set_extension(#drivermsg{
		type = 'DATA',
		synnum = SynNum},
		get_location, true),
	

	MsgList = location_pb:encode_drivermsg(Msg),
	HdrList = amber_client_msg:build_header(6, 0),

	amber_client_msg:send_message(HdrList, MsgList, Socket, Address, Port), 

	Reply = case amber_client_msg:receive_msg(Socket) of
		{ok, Packet} -> 
			LocationMsg = location_pb:decode_drivermsg(Packet),
			SynNum = LocationMsg#drivermsg.acknum,
			{ok, Location} = location_pb:get_extension(LocationMsg, currentlocation),
			Location;
		{error, Reason} ->
			{error, Reason}
	end,

	{reply, Reply, State#state{synnum = SynNum + 1}}.
	

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

handle_info(_Info, State) ->
    {noreply, State}.
