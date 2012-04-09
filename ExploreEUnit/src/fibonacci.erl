%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, 
%%% @doc
%%% fibonacci code to explore EUnit
%%% @end
%%% Created :  2 Dec 2011 by  <sps@thyrsus-laptop2>

-module(fibonacci).

-export([fib/1]).
-include("global.hrl").

fib(0) ->
    1;
fib(1) ->
    1;
fib(N) when N > 1 ->
    fib(N-1) + fib(N-2).
