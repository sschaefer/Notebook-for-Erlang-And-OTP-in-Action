%%% @author Stephen P. Schaefer <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Tests for the die_please2 module.
%%% @end
%%% Created : 17 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(die_please2_tests).

-include_lib("eunit/include/eunit.hrl").

-spec go_test() -> ok.
go_test() ->
    ?assertError({badmatch, right_now}, die_please2:go()).
