%%%-------------------------------------------------------------------
%%% @author  Logan, Merritt, and Carlsson
%%% @copyright (C) 2012, 
%%% @doc
%%% Simple Cache event stream API
%%% @end
%%% Created : 28 Jan 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(sc_event).

%% API
-export([start_link/0,
	 add_handler/2,
	 delete_handler/2,
	 lookup/1,
	 create/2,
	 replace/2,
	 delete/1]).

-define(SERVER, ?MODULE). 

-include("sc_event.hrl").

%%--------------------------------------------------------------------
%% @doc
%% API: Creates an event manager
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, Error :: term()}.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% API: Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec add_handler(Handler :: atom(), Args :: list()) ->
			 ok | {'EXIT', Reason :: term()} | term().
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% API: Delete event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_handler(Handler :: atom(), Args :: list()) ->
			    term() | {error, module_not_found} | {'EXIT', Reason :: term()}.
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% API: Handle lookup event
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup(Key :: term()) -> ok.
lookup(Key) ->
    gen_event:notify(?SERVER, {lookup, Key}).

%%--------------------------------------------------------------------
%% @doc
%% API: Handle create event
%%
%% @end
%%--------------------------------------------------------------------
-spec create(Key :: term(), Value :: term()) -> ok.
create(Key, Value) ->
    gen_event:notify(?SERVER, {create, {Key, Value}}).

%%--------------------------------------------------------------------
%% @doc
%% API: Handle replace event
%%
%% @end
%%--------------------------------------------------------------------
-spec replace(Key :: term(), Value :: term()) -> ok.
replace(Key, Value) ->
    gen_event:notify(?SERVER, {replace, {Key, Value}}).

%%--------------------------------------------------------------------
%% @doc
%% API: Handle delete event
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(Key :: term()) -> ok.
delete(Key) ->
    gen_event:notify(?SERVER, {delete, Key}).
