-module(performance_test).

-export[start/0].

-define(REPEATS, 5000).

start() ->
	net_kernel:connect_node(roboss@rose),
	Before = erlang:now(),
	loop(Before, ?REPEATS).

loop(Before, 0) ->
	After = erlang:now(),
	Diff = timer:now_diff(After, Before),
	io:format("request_state: ~wus~n", [Diff/?REPEATS]);

loop(Before, Counter) ->
	roboss_serv:request_state("robot1"),
	loop(Before, Counter - 1).


