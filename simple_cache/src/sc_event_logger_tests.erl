%%%-------------------------------------------------------------------
%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%% Test of sc_event_logger module
%%% @end
%%% Created : 31 Jan 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(sc_event_logger_tests).

%% API
-export([]).

-include("sc_event_logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% None
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

% the add_ and delete_ handler tests can only work if the sc_event is operating (might need sc_sup, also).
add_delete_handler_test_() ->
    {setup,
      fun() -> gen_event:start({local, sc_event}) end,
      fun(_) -> gen_event:stop(sc_event) end,
      [?_assertEqual(ok, sc_event_logger:add_handler()),
       ?_assertEqual(ok, sc_event_logger:delete_handler())]}.

% to test this event logger, we need to capture the events sent to error_logger and
% determine that they are as intended.

% build a process to store the most recent event sent to error_logger
handle_event_test_() ->
    {setup,
     fun() -> test_error_logging:register_with_logger() end,
     [?_assertEqual(
	 {{ok, #state{}}, {"create(~w, ~w)~n", [one, two]}},
	 {sc_event_logger:handle_event({create, {one, two}}, #state{}),
	  test_error_logging:retrieve_info_msg()}),
      ?_assertEqual(
	 {{ok, #state{}}, {"lookup(~w)~n", [one]}},
	 {sc_event_logger:handle_event({lookup, one}, #state{}),
	  test_error_logging:retrieve_info_msg()}),
      ?_assertEqual(
	 {{ok, #state{}}, {"delete(~w)~n", [one]}},
	 {sc_event_logger:handle_event({delete, one}, #state{}),
	  test_error_logging:retrieve_info_msg()}),
      ?_assertEqual(
	 {{ok, #state{}}, {"replace(~w, ~w)~n", [one, three]}},
	 {sc_event_logger:handle_event({replace, {one, three}}, #state{}),
	  test_error_logging:retrieve_info_msg()}),
      ?_assertError(function_clause, sc_event_logger:handle_event({never_defined}, #state{}))
     ]}.
