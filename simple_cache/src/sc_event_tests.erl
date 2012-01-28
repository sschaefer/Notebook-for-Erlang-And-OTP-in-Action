%%%-------------------------------------------------------------------
%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------

%%%%%%%%%%
%%%%%%%%%%
%%%%%%%%%% The add_handler routine under test must refer to a module implementing
%%%%%%%%%% the gen_event behavior, so this test module must provide that
%%%%%%%%%%
%%%%%%%%%%

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
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([handler_args]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(Event, _State) ->
    {ok, #state{last_event = Event}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = {ok, State},
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
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


