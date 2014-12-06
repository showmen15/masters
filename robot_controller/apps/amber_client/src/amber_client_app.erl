-module(amber_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:load(client),
	amber_client_sup:start_link().
	%net_kernel:connect(RobossNode),
	%global:sync(),

	%case roboss_serv:is_alive() of
	%	false ->  
	%		{stop, "Roboss node not connected"};
	%	true ->
	%		roboss_client_sup:start_link()
	%end.    

stop(_State) ->
    ok.