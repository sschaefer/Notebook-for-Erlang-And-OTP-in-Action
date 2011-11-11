%%%------------------------------------------------------------------------------
%%% @author Stephen P. Schaefer <sschaefer@acm.org>
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Eunit tests for sc_sup module provided in
%%%      the "OTP in Action" book by Logan, Merritt and Carlsson.
%%%
%%% @end
%%%------------------------------------------------------------------------------
-module(sc_sup_test).

-include("eunit-2.1.7/include/eunit.hrl").

-export([]).

-type child() :: pid() | undefined.
-type startchild_ret() :: {ok, Child :: child()}
                 | {ok, Child :: child(), Info :: term()}
                 | {error, startchild_err()}.
-type startchild_err() :: already_present
                 | {already_started, Child :: child()}
                 | term().

-spec start_link_test() -> {ok, pid()}.
start_link_test() ->
    {ok,_Pid} = sc_sup:start_link().

-spec start_child_test() -> startchild_ret().
start_child_test() ->
    {ok, _Pid} = sc_sup:start_child(value, lease_time).

-type strategy() :: one_for_all
           | one_for_one
           | rest_for_one
           | simple_one_for_one.

-type restart() :: permanent | transient | temporary.

-type shutdown() :: brutal_kill | timeout().

-type worker() :: worker | supervisor.

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
      }} = sc_sup:init([]).




