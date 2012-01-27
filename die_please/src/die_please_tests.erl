%%% @author Stephen P. Schaefer <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Tests for the die_please module.
%%% @end
%%% Created : 17 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(die_please_tests).

-include("die_please.hrl").

-spec start_link_test() -> ok.
start_link_test() ->
    {ok, Pid} = die_please:start_link().

-spec init_test() -> ok.
init_test() ->
    ?assertMatch({ok, #state{}, 2000}, die_please:init([])).

-spec handle_call_test() -> ok.
handle_call_test() ->
    ?assertMatch({reply, ok, #state{}}, die_please:handle_call(ignored, ignored, #state{})).

-spec handle_cast_test() -> ok.
handle_cast_test() ->
    ?assertMatch({noreply, #state{}}, die_please:handle_cast(ignored, #state{})).

-spec handle_info_test() -> ok.
handle_info_test() ->
    ?assertError({badmatch, right_now}, die_please:handle_info(timeout, #state{})).

-spec terminate_test() -> ok.
terminate_test() ->
    ?assertMatch(ok, die_please:terminate(ignored1, ignored2)).

-spec code_change_test() -> ok.
code_change_test() ->
    ?assertMatch({ok, #state{}}, die_please:code_change(ignored1, #state{}, ignored2)).

