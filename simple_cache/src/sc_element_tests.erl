%%% @author Stephen P. Schaefer <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Test routines for simple_cache:sc_element module
%%% @end
%%% Created : 10 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_element_tests).

-include("sc_element.hrl").

-export([]).

-spec start_link_test() -> {ok, pid()}.
start_link_test() ->
    %%% a side effect for ALL tests sc_store:init() call required to initialize required module sc_store 
    ?assertEqual(ok, sc_store:init()),
    {ok, _Pid} = sc_element:start_link(value1, 60).

create2_test() ->
    sc_element_sup:start_link(), % elements invoke supervisor resources
    {ok, _Pid} = sc_element:create(value2, 60).

create1_test() ->
    {ok, _Pid} = sc_element:create(value3).

fetch_test() ->
    {ok, Pid} = sc_element:create(value4),
    {ok, value4} = sc_element:fetch(Pid).

replace_test() ->
    {ok, Pid} = sc_element:create(value1),
    ok = sc_element:replace(Pid, value2),
    {ok, value2} = sc_element:fetch(Pid).

delete_test() ->
    {ok, Pid} = sc_element:create(value5),
    ok = sc_element:delete(Pid).
% check that Pid no longer exists

init_test() ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    AfterCurrentTime = CurrentTime + 1,
    {ok, #state{value = value1, start_time = StartTime, lease_time = 0}, 0} =
	sc_element:init([value1, 0]),
    ?assert(CurrentTime =< StartTime),
    ?assert(StartTime =< AfterCurrentTime).    

handle_call_test() ->
    State = #state{value = value1, lease_time = 0, start_time = 0},
    {reply, {ok, value1}, State, 0} =
	sc_element:handle_call(fetch, ignored, State).
	   

handle_cast_test() ->
    ?assertMatch({noreply,
		  #state{value = value2, lease_time = 0, start_time = 0}, 0},
		 sc_element:handle_cast({replace, value2},
					#state{value = value1, lease_time = 0, start_time = 0})),
    ?assertMatch({stop, normal, state}, sc_element:handle_cast(delete, state)).

handle_info_test() ->
    ?assertMatch({stop, normal, state}, sc_element:handle_info(timeout, state)).

terminate_test() ->
    ?assertEqual(true, sc_store:insert(terminate_test, self())),
    ?assertEqual(ok, sc_element:terminate(ignored1, ignored2)).

code_change_test() ->
    {ok, state} = sc_element:code_change(ignored1, state, ignored2).

cleanup_test() ->
    ets:delete(sc_store).
