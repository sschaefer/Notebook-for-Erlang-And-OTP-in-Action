%%% @author Stephen P. Schaefer <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Unit tests for the sc_store module.
%%% @end
%%% Created : 12 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_store_tests).

-include("sc_store.hrl").

-spec init_test() -> ok.
init_test() ->
    ok = sc_store:init().

-spec insert_test() -> {ok, value2}.
insert_test() ->
    true = sc_store:insert(value1, value2),
    {ok, value2} = sc_store:lookup(value1).

-spec delete_test() -> true.
delete_test() ->
    true = sc_store:insert(value1, value2),
    true = sc_store:delete(value2),
    {error, not_found} = sc_store:lookup(value1).

-spec lookup_test() -> [{value1, value2}].
lookup_test() ->
    {error, not_found} = sc_store:lookup(no_such_value),
    true = sc_store:insert(value1, value2),
    {ok, value2} = sc_store:lookup(value1).
