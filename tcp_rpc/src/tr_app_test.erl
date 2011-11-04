%%%------------------------------------------------------------------------------
%%% @author Stephen P. Schaefer <sschaefer@acm.org>
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Eunit tests for tr_app module provided in
%%%      the "OTP in Action" book by Logan, Merritt and Carlsson.
%%%
%%% @end
%%%------------------------------------------------------------------------------
-module(tr_app_test).

-include("eunit-2.1.7/include/eunit.hrl").

-export([]).

start_test() ->
    {ok, _Pid} = tr_app:start(type, startargs).

stop_test() ->
    ok = tr_app:stop(test).

