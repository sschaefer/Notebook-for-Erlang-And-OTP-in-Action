%%% @author Logan, Merritt and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% sc_element module of simple_cache, storing time limited values in processes
%%% @end
%%% Created : 10 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_element).

-behavior(gen_server).

-export([
	 start_link/2,
	 create/2,
	 create/1,
	 fetch/1,
	 replace/2,
	 delete/1
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).

start_link(Value, LeaseTime) ->
    bad.

create(Value, LeaseTime) ->
    bad.

create(Value) ->
    bad.

fetch(Pid) ->
    bad.

replace(Pid, Value) ->
    bad.

delete(Pid) ->
    bad.

init([Value, LeaseTime]) ->
    bad.

handle_call(fetch, _From, State) ->
    bad.

handle_cast({replace, Value}, State) ->
    bad;

handle_cast(delete, State) ->
    bad.

handle_info(timeout, State) ->
    bad.

terminate(_Reason, _State) ->
    bad.

code_change(_OldVsn, State, _Extra) ->
    bad.
