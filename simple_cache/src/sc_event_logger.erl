%%%-------------------------------------------------------------------
%%% @author  Logan, Merritt, and Carlsson
%%% @copyright (C) 2012, 
%%% @doc
%%% - transcribed and modified by Stephen P. Schaefer
%%% @end
%%% Created : 31 Jan 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(sc_event_logger).

-behaviour(gen_event).

%% API
%-export([start_link/0, add_handler/0, delete_handler/0]).
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("sc_event_logger.hrl").

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

-ifdef(false).
%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager: boilerplate unused in this case
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link({local, ?SERVER}).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% API: Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec add_handler() -> ok | {'EXIT', Reason::term()} | term().
add_handler() ->
    sc_event:add_handler(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% API: Deletes an event handler
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete_handler() -> ok.
delete_handler() ->
    sc_event:delete_handler(?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_event({create, {Key :: term(), Value :: term()}} |
		   {lookup, Key :: term()} |
		   {delete, Key :: term()} |
		   {replace, {Key :: term(), Value :: term()}}, State :: #state{}) ->
			  {ok, State}.
handle_event({create, {Key, Value}}, State) ->
    error_logger:info_msg("create(~w, ~w)~n", [Key, Value]),
    {ok, State};
handle_event({lookup, Key}, State) ->
    error_logger:info_msg("lookup(~w)~n", [Key]),
    {ok, State};
handle_event({delete, Key}, State) ->
    error_logger:info_msg("delete(~w)~n", [Key]),
    {ok, State};
handle_event({replace, {Key, Value}}, State) ->
    error_logger:info_msg("replace(~w, ~w)~n", [Key, Value]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), State :: #state{}) -> {ok, ok, State}.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% gen_event callback: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #state{}) -> {ok, State}.
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: #state{}) -> void().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) -> {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
