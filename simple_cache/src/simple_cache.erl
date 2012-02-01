%%% @author Logan, Merritt, and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% Public API to simple_cache application
%%% - transcribed and modified by Stephen P. Schaefer
%%% @end
%%% Created : 12 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(simple_cache).

-include("simple_cache.hrl").

-export([insert/2, lookup/1, delete/1]).

%%%================================================================
%%% API
%%%================================================================

%% @doc Install a key and its value into the cache.
-spec insert(term(), term()) -> true.
insert(Key, Value) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_event:replace(Key, Value),
	    sc_element:replace(Pid, Value);
	{error, _} ->
	    sc_event:create(Key, Value),
	    {ok, Pid} = sc_element:create(Value),
	    sc_store:insert(Key, Pid)
    end.

%% @doc Look up a value given its key.
-spec lookup(term()) -> {ok, term()}.
lookup(Key) ->
    sc_event:lookup(Key),
    try
	{ok, Pid} = sc_store:lookup(Key),
	{ok, Value} = sc_element:fetch(Pid),
	{ok, Value}
    catch
	_Class:_Exception ->
	    {error, not_found}
    end.

%% @doc delete a key and its value from the cache.
-spec delete(term()) -> ok.
delete(Key) ->
    sc_event:delete(Key),
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:delete(Pid);
	{error, _Reason} ->
	    ok
    end.
