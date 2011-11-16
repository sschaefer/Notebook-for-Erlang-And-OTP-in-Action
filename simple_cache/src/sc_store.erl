%%% @author Logan, Merritt and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% Map keys to process ids.
%%% @end
%%% Created : 12 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_store).

-include("sc_store.hrl").

-export([
	 init/0,
	 insert/2,
	 delete/1,
	 lookup/1
	]).

-define(TABLE_ID, ?MODULE).

%% @doc Acquire an ETS table to store keys and their corresponding process IDs.
-spec init() -> etsnew_ret().
init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

%% @doc Add a key and its corresponding process ID to the store.
-spec insert(term(), pid()) -> true.
insert(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

%% @doc Given a key, return the process ID.
-spec lookup(term()) -> {ok|error, pid()|not_found}.
lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
	[{Key, Pid}] -> {ok, Pid};     
	[]           -> {error, not_found}
    end.

%% @doc Delete the key and process ID.
-spec delete(pid()) -> true.
delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_',Pid}).
