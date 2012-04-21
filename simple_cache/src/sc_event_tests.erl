%%%-------------------------------------------------------------------
%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%% The add_handler routine under test must refer to a module implementing
%%% the gen_event behavior, so this test module must provide that.
%%% @end
%%% Created : 28 Jan 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------

-module(sc_event_tests).

-behaviour(gen_event).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {last_event}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% API: Creates an event manager
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, {already_started,pid()}}.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% API: Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec add_handler() -> ok | {'EXIT', Reason :: term } | term().
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

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
-spec init([handler_args]) -> {ok, #state{}}.
init([handler_args]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event :: term(), State :: #state{}) -> {ok, #state{}}.
handle_event(Event, _State) ->
    {ok, #state{last_event = Event}}.

%%--------------------------------------------------------------------
%% @doc
%% gen_event callback: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), #state{}) -> {ok, ok, #state{}}.
handle_call(_Request, State) ->
    Reply = {ok, State},
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
-spec terminate(Reason :: term(), #state{}) -> ok.
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


%%%%%%%%%%
%%%%%%%%%%
%%%%%%%%%% Everything above is template.  Now the tests.
%%%%%%%%%%
%%%%%%%%%%

%%%===================================================================
%%% TESTS
%%%===================================================================

start_link_test() ->
    ?assertEqual(sc_event:start_link(), {ok, whereis(sc_event)}).

add_handler_test() ->
    ?assertEqual(ok, sc_event:add_handler(?MODULE, [handler_args])),
    ?assertEqual(present, query_event_handlers(?MODULE)).

delete_handler_test() ->
    ?assertEqual(ok, sc_event:delete_handler(?MODULE, [handler_args])),
    ?assertEqual(missing, query_event_handlers(?MODULE)).

lookup1_test() ->
    ?assertEqual(ok, sc_event:lookup(not_there)).
lookup2_test_() ->
    { setup,
      fun() -> sc_event:add_handler(?MODULE, [handler_args]) end, % setup
      fun(_) -> sc_event:delete_handler(?MODULE, [handler_args]) end, % cleanup
      [ ?_assertEqual({ok,
                       {ok, #state{last_event = {lookup, not_there}}}
                      },
		      {sc_event:lookup(not_there),
		       gen_event:call(sc_event, ?MODULE, any)
		      })]
    }.

create1_test() ->
    ?assertEqual(ok, sc_event:create(key1, value1)).
create2_test_() ->
    { setup,
      fun() -> sc_event:add_handler(?MODULE, [handler_args]) end, % setup
      fun(_) -> sc_event:delete_handler(?MODULE, [handler_args]) end, % cleanup
      [ ?_assertEqual({ok,
		       {ok, #state{last_event = {create, {key2, value2}}}}
		      },
		      {sc_event:create(key2, value2),
		       gen_event:call(sc_event, ?MODULE, any)
		      }) ]
    }.

replace1_test() ->
    ?assertEqual(ok, sc_event:replace(key3, value3)).
replace2_test_() ->
    { setup,
      fun() -> sc_event:add_handler(?MODULE, [handler_args]) end, % setup
      fun(_) -> sc_event:delete_handler(?MODULE, [handler_args]) end, % cleanup
      [ ?_assertEqual({ok,
		       {ok, #state{last_event = {replace, {key4, value4}}}}
		       },
		      {sc_event:replace(key4, value4),
		       gen_event:call(sc_event, ?MODULE, any)
		      }) ]
    }.

delete1_test() ->
    ?assertEqual(ok, sc_event:delete(key5)).
delete2_test_() ->
    { setup,
      fun() -> sc_event:add_handler(?MODULE, [handler_args]) end, % setup
      fun(_) -> sc_event:delete_handler(?MODULE, [handler_args]) end, % cleanup
      [ ?_assertEqual({ok,
		       {ok, #state{last_event = {delete, key6}}}
		      },
		      {sc_event:delete(key6),
		       gen_event:call(sc_event, ?MODULE, any)
		      }) ]
    }.

query_event_handlers(Handler) ->
    Handlers = gen_event:which_handlers(whereis(sc_event)),
    case lists:member(Handler, Handlers) of
	true ->
	    present;
	false ->
	    missing
    end.


