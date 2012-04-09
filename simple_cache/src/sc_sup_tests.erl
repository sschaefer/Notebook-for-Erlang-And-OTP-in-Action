%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%% tests for Erlang and OTP in Action Chapter 7 sc_sup.
%%% @end
%%% Created : 28 Jan 2012 by  <sps@thyrsus-laptop2>

-module(sc_sup_tests).

-include_lib("eunit/include/eunit.hrl").
-include("sup.hrl").

-spec start_link_test() -> {ok, pid()}.
start_link_test() ->
    {ok, _Pid} = sc_sup:start_link().

init_test() ->
    ?assertEqual({ok, {
       {one_for_one, 4, 3600},
       [ {sc_element_sup, {sc_element_sup, start_link, []}, permanent, 2000, supervisor, [sc_element_sup]},
	 {sc_event, {sc_event, start_link, []}, permanent, 2000, worker, dynamic}] }},
		  sc_sup:init([])).
