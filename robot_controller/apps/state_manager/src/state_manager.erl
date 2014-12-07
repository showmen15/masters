-module(state_manager).

-behaviour(gen_server).

-include("include/records.hrl").

-export([start_link/0, stop/0, update/2, get_states/0, evict/0, start_evicter/0, evicter/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	states = dict:new(),
	evict_period,
	eviction_age
	}).

%% Public API

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call({global, ?MODULE}, stop).

update(RobotName, {_X, _Y, _Theta, _FF, _Timestamp} = RobotState) ->
	gen_server:cast({global, ?MODULE}, {update, RobotName, RobotState, self()}).

get_states() ->
	gen_server:call({global, ?MODULE}, get_states).

evict() ->
	gen_server:cast({global, ?MODULE}, evict).

start_evicter() ->
	{ok, EvictPeriod} = application:get_env(state_manager, evict_period),
	io:format("evicter started~n"),
	Pid = spawn_link(?MODULE, evicter, [EvictPeriod]),
	{ok, Pid}.

evicter(EvictPeriod) ->
	receive
	after EvictPeriod ->
		state_manager:evict()
	end,
	evicter(EvictPeriod).

%% Callbacks

init(_Args) ->
	{ok, EvictionAge} = application:get_env(state_manager, eviction_age),

	io:format("~s started~n", [?MODULE]),

	{ok, #state{
		eviction_age = EvictionAge * 1000
	}}.

handle_call(get_states, _From, #state{states = RobotStates} = State) -> 
	{reply, RobotStates, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({update, RobotName, {X, Y, Theta, FF, Timestamp}, From}, #state{states = OldStates} = State) ->
	RMS = #robot_manager_state{
		x = X,
		y = Y,
		theta = Theta,
		ff = FF,
		timestamp = Timestamp,
		from = From,
		added = erlang:now()
	},

	case dict:is_key(RobotName, OldStates) of
		false -> io:format("~s added~n", [RobotName]);
		_ -> true
	end,

	NewStates = dict:store(RobotName, RMS, OldStates),
	{noreply, State#state{states = NewStates}};

handle_cast(evict, #state{states = RobotStates, eviction_age = EvictionAge} = State) ->
	Now = erlang:now(),
	Pred = fun (RobotName, RobotState) ->
		Added = RobotState#robot_manager_state.added,
		Ret = timer:now_diff(Now, Added) =< EvictionAge,
		case Ret of 
			false -> io:format("~s evicted~n", [RobotName]);
			_ -> true
		end,
		Ret
	end,

	NewStates = dict:filter(Pred, RobotStates),
	{noreply, State#state{states = NewStates}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	io:format("info: ~w ~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	