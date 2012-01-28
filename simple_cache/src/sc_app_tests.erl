%%% @author Stephen P. Schaefer <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Tests for sc_app of the simple_cache application.
%%% @end
%%% Created :  6 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_app_tests).

-include_lib("eunit/include/eunit.hrl").

-export([]).

-spec start_test() -> {ok, pid()}.
start_test() ->
    {ok, _Pid} = sc_app:start(ignored, ignored),
   '$end_of_table' = ets:first(sc_store).

-spec stop_test() -> ok.
stop_test() ->
    ok = sc_app:stop(ignored).
