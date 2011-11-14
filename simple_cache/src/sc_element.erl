%%% @author Logan, Merritt and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% sc_element module of simple_cache, storing time limited values in processes
%%% @end
%%% Created : 10 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_element).

-behavior(gen_server).

-export([
	 start_link/2,
	 create/2,
	 create/1,
	 fetch/1,
	 replace/2,
	 delete/1
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-include("sc_element.hrl").

%%%================================================================
%%% API
%%%================================================================

%% @doc explain
-spec start_link(term(), time_period()) -> startlink_ret().
start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%% @doc explain
-spec create(term(), time_period()) -> startchild_ret().
create(Value, LeaseTime) ->
    sc_sup:start_child(Value, LeaseTime).

%% @doc explain
-spec create(term()) -> startchild_ret().
create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

%% @doc explain
-spec fetch(pid()) -> ok.
fetch(Pid) ->
    gen_server:call(Pid, fetch).

%% @doc explain
-spec replace(pid(), term()) -> ok.
replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

%% @doc explain
-spec delete(pid()) -> ok.
delete(Pid) ->
    gen_server:cast(Pid, delete).

%%%================================================================
%%% Callbacks
%%%================================================================

%% @doc explain
-spec init([term()]) -> {ok, element_state(), time_period()}.
init([Value, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {ok,
     #state{value = Value,
	    start_time = StartTime,
	    lease_time = LeaseTime},
     time_left(StartTime, LeaseTime)}.

%% @doc explain
-spec handle_call(fetch, _, element_state()) -> {reply, {ok, term()}, time_period()}.
handle_call(fetch, _From, State) ->
    #state{value = Value,
	   lease_time = LeaseTime,
	   start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Value}, State, TimeLeft}.

%% @doc explain
-spec handle_cast({replace, term()}|delete, element_state()) -> {noreply|stop, element_state(), time_period()|element_state()}.
handle_cast({replace, Value}, State) ->
    #state{lease_time = LeaseTime,
	   start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};

handle_cast(delete, State) ->
    {stop, normal, State}.

%% @doc explain
-spec handle_info(timeout, element_state()) -> {stop, normal, element_state()}.
handle_info(timeout, State) ->
    {stop, normal, State}.

%% @doc explain
-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    sc_store:delete(self()),
    ok.

%% @doc explain
-spec code_change(_, element_state(), _) -> {ok, element_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%================================================================
%%% Internal functions
%%%================================================================

-spec time_left(integer(), time_period()) -> integer().
time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
	Time when Time =< 0 -> 0;
	Time                -> Time * 1000
    end.

time_left_test() ->
    infinity = time_left(ignored, infinity),
    Now = calendar:local_time(),
    0 = time_left(0, 0),
    0 = time_left(0, calendar:datetime_to_gregorian_seconds(Now) - 1),
    ?assert(0 < time_left(0, calendar:datetime_to_gregorian_seconds(Now) + 1)).
