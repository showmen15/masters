-module(roboss_client_udp_serv).

-export([start_link/0, init/0]).

-define(UDP_PORT, 9015).

start_link() ->
	{ok, spawn_link(?MODULE, init, [])}.
	
init() -> 
	{ok, Socket} = gen_udp:open(?UDP_PORT),
	loop(Socket).

loop(Socket) ->
	receive 
		{udp, Socket, _FromIp, _FromPort, Message} ->
			handle_message(Message)
	end,
	loop(Socket).

handle_message("start") ->
	io:format("Start simulation~n"),
	roboss_serv:start_simulation(),
	client_controllers_sup:set_event(start);

handle_message("stop") ->
	io:format("Stop simulation~n"),
	roboss_serv:stop_simulation(),
	client_controllers_sup:set_event(stop);

handle_message("reset") ->
	io:format("Reset simulation~n"),
	roboss_serv:reset_simulation();
	
handle_message(Msg) ->
	io:format("Unrecognised UDP message: ~s~n", [Msg]).