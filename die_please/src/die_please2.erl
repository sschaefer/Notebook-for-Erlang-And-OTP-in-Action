%%% @author Logan, Merritt and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% Illustrate SASL used without gen_server behavior
%%% @end
%%% Created : 17 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(die_please2).

-export([go/0]).

-define(SLEEP_TIME, 2000).

%% @doc: sleep for a period, then throw a badmatch error
go() ->
    %% just sleep for a while, then crash
    timer:sleep(?SLEEP_TIME),
    i_really_want_to_die = right_now.
