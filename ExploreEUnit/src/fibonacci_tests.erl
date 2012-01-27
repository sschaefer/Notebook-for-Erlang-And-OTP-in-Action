%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, 
%%% @doc
%%% fibonnaci_test - explore EUnit
%%% @end
%%% Created :  2 Dec 2011 by  <sps@thyrsus-laptop2>

-module(fibonacci_tests).

-include("global.hrl").

f_test_() ->
    [?_assertEqual(1, fibonacci:fib(0)),
     ?_assertEqual(1, fibonacci:fib(1)),
     ?_assertEqual(2, fibonacci:fib(2)),
     ?_assertError(function_clause, fibonacci:fib(-1)),
     ?_assert(2178309 =:= fibonacci:fib(31))].

