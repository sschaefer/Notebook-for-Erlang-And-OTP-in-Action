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
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

% the add_ and delete_ handler tests can only work if the sc_event is operating (might need sc_sup, also).
add_handler_test_() ->
    {setup,
      fun() -> sc_event:start_link() end,
      ?_assertEqual(ok, sc_event_logger:add_handler())}.

delete_handler_test_() ->
    {setup,
      fun() -> sc_event:start_link() end,
     ?_assertEqual(ok, sc_event_logger:delete_handler())}.

handle_event_test_() ->
    [?_assertEqual(bad, sc_event_logger:handle_event({create, {one, two}}, #state{})),
     ?_assertEqual(bad, sc_event_logger:handle_event({create, {one, alredy_there}})),
     ?_assertEqual(bad, sc_event_logger:handle_event({lookup, one}, #state{})),
     ?_assertEqual(bad, sc_event_looger:handle_event({delete, one}, #state{})),
     ?_assertEqual(bad, sc_event_logger:handle_event({delete, not_there}, #state{})),
     ?_assertEqual(bad, sc_event_logger:handle_event({replace, {one, three}}, #state{})),
     ?_assertEqual(bad, sc_event_logger:handle_event({replace, {not_there, four}}, #state{}))
    ].
		  
