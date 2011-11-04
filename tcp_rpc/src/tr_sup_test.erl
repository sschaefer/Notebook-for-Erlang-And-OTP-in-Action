%%%------------------------------------------------------------------------------
%%% @author Stephen P. Schaefer <sschaefer@acm.org>
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Eunit tests for tr_sup module provided in
%%%      the "OTP in Action" book by Logan, Merritt and Carlsson.
%%%
%%% @end
%%%------------------------------------------------------------------------------
-module(tr_sup_test).

-include("eunit-2.1.7/include/eunit.hrl").

-export([]).

start_link_test() ->
    {ok,_Pid} = tr_sup:start_link().

init_test() ->
    {ok, {
       {one_for_one, 0, 1},
       [{tr_server,
	 {tr_server,start_link, []},
	 permanent,
	 2000,
	 worker,
	 [tr_server]
	}]
      }}.




