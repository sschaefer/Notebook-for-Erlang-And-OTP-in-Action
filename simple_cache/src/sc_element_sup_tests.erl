%%%------------------------------------------------------------------------------
%%% @author Stephen P. Schaefer <sschaefer@acm.org>
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Eunit tests for sc_element_sup module provided in
%%%      the "Erlang and OTP in Action" book by Logan, Merritt and Carlsson.
%%%
%%% @end
%%%------------------------------------------------------------------------------
-module(sc_element_sup_tests).

-include_lib("eunit/include/eunit.hrl").
-include("sup.hrl").

-spec start_link_test() -> {ok, pid()}.
start_link_test() ->
    {ok,_Pid} = sc_element_sup:start_link().

-spec start_child_test() -> startchild_ret().
start_child_test() ->
    {ok, _Pid} = sc_element_sup:start_child(value, 60).

-spec init_test() -> {ok, {
       {strategy(), 0, 1},
       [{sc_element,
	 {sc_element,start_link, []},
	 restart(),
	 shutdown(),
	 worker(),
	 [sc_element]
	}]
      }}.
init_test() ->
    {ok, {
       {simple_one_for_one, 0, 1},
       [{sc_element,
	 {sc_element,start_link, []},
	 temporary,
	 brutal_kill,
	 worker,
	 [sc_element]
	}]
      }} = sc_element_sup:init([]).




