-module(amber_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:load(client),
	{ok, StateManager} = application:get_env(state_manager_node),
	net_kernel:connect(StateManager),
	global:sync(),

	case state_manager:is_alive() of
		false ->  
			{error, "state_manager node not connected"};
		true ->
			amber_client_sup:start_link()
	end.    

stop(_State) ->
    ok.
