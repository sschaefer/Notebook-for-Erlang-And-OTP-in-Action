%%%-------------------------------------------------------------------
%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%% Capture event sent to error logger
%%% @end
%%% Created :  1 Feb 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(test_error_logging).

-behaviour(gen_event).

%% API
-export([register_with_logger/0,retrieve_info_msg/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {pid, first_arg, second_arg}).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% API: Tell error_logger to start sending us events
%% @end
%%--------------------------------------------------------------------
-spec register_with_logger() -> ok.
register_with_logger() ->
    error_logger:add_report_handler(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% API: Obtain info message sent to error_logger.
%% @end
%%--------------------------------------------------------------------
-spec retrieve_info_msg() -> Result :: term().
retrieve_info_msg() ->
    gen_event:call(error_logger, ?MODULE, retrieve_info_msg).
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
-spec handle_event(Event :: term(), State :: #state{}) -> {ok, #state{}}.
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, _State) ->
    {ok, #state{pid=Pid, first_arg=Format, second_arg=Data}};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term, #state{}) -> {ok, Reply :: term(), #state{}}.
handle_call(retrieve_info_msg, State) ->
    #state{pid=_Pid, first_arg=Format, second_arg=Data} = State,
    {ok, {Format, Data}, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), #state{}) -> {ok, #state{}}.
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
-spec terminate(Reason :: term(), #state{}) -> void().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), #state{}, Extra :: term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
