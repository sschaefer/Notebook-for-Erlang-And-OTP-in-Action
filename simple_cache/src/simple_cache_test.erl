%%% @author Stephen P. Schaefer <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Test the functions provided by module simple_cache.
%%% @end
%%% Created : 12 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(simple_cache_test).

-include("simple_cache.hrl").

-spec insert_test() -> ok.
insert_test() ->
    %%% a side effect for ALL tests sc_app:start/2 call required to initialize required modules
    {ok, _Pid} = sc_app:start(ignored1, ignored2),
    true = simple_cache:insert(key10, value10).

-spec lookup_test() -> {ok, value30}.
lookup_test() ->
    {error, not_found} = simple_cache:lookup(key20),
    true = simple_cache:insert(key30, value30),
    {ok, value30} = simple_cache:lookup(key30).

-spec delete_test() -> {error, not_found}.
delete_test() ->
    true = simple_cache:insert(key40, value40),
    {ok, value40} = simple_cache:lookup(key40),
    ok = simple_cache:delete(key40),
    {error, not_found} = simple_cache:lookup(key40),
    ok = simple_cache:delete(key40).
