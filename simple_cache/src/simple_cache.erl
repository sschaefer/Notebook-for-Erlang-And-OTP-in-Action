%%% @author Logan, Merritt, and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% Public API to simple_cache application
%%% @end
%%% Created : 12 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(simple_cache).

-include("simple_cache.hrl").

-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:replace(Pid, Value);
	{error, _} ->
	    {ok, Pid} = sc_element:create(Value),
	    sc_store:insert(Key, Pid)
    end.

lookup(Key) ->
    try
	{ok, Pid} = sc_store:lookup(Key),
	{ok, Value} = sc_element:fetch(Pid),
	{ok, Value}
    catch
	_Class:_Exception ->
	    {error, not_found}
    end.

delete(Key) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:delete(Pid);
	{error, _Reason} ->
	    ok
    end.
