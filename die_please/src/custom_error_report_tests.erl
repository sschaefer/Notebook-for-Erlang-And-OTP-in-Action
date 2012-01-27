%%%-------------------------------------------------------------------
%%% @author Stephen P. Schaefer
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Test functions for demonstration of a custom event handler.
%%% @end
%%% Created : 19 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(custom_error_report_tests).

%% API

-define(SERVER, ?MODULE). 

-include("custom_error_report.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Test of registration with logger.  The only (documented) way to tell
%% whether a handler is registered is to attempt to delete it.
%% @end
%%--------------------------------------------------------------------
%%
-spec register_with_logger_test() -> ok.
register_with_logger_test() ->
    ?assertMatch({error, module_not_found}, error_logger:delete_report_handler(custom_error_report)),
    ?assertMatch(ok, custom_error_report:register_with_logger()),
    ?assertMatch(ok, error_logger:delete_report_handler(custom_error_report)).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
init_test() ->
    ?assertMatch({ok, #state{}}, custom_error_report:init([])).

%%--------------------------------------------------------------------
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
io_helper(Function) ->
    IO = stdout_test:capture(),
    Value = Function(),
    stdout_test:release(IO),
    Output = stdout_test:stdout(IO),
    stdout_test:finish(IO),
    {Value, Output}.

str_format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

handle_event01_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("ERROR <~p>", [self()]) ++ " format data1 string\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {error,
			       ignored,
			       {self(), "format ~s string", ["data1"]}},
			      #state{}) end)).
handle_event02_test() ->
    ?assertEqual({{ok, #state{}},
		   str_format("ERROR <~p>", [self()]) ++ " \"report1 string\"\n"},
		  io_helper(
		    fun() -> custom_error_report:handle_event(
			       {error_report,
				ignored,
				{self(), std_error, "report1 string"}},
			       #state{}) end)).
handle_event03_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("ERROR <~p>", [self()]) ++ " type \"report2 string\"\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {error_report,
			       ignored,
			       {self(), type, "report2 string"}},
			      #state{}) end)).
handle_event04_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("WARNING <~p>", [self()]) ++ " format data2 string\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {warning_msg,
			       ignored,
			       {self(), "format ~s string", ["data2"]}},
			      #state{}) end)).
handle_event05_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("WARNING <~p>", [self()]) ++ " \"report3 string\"\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {warning_report,
			       ignored,
			       {self(), std_warning, "report3 string"}},
			      #state{}) end)).
handle_event06_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("WARNING <~p>", [self()]) ++ " type \"report 4\"\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {warning_report,
			       ignored,
			       {self(), type, "report 4"}},
			      #state{}) end)).
handle_event07_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("INFO <~p>", [self()]) ++ " format data3 string\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {info_msg,
			       ignored,
			       {self(), "format ~s string", ["data3"]}},
			      #state{}) end)).
handle_event08_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("INFO <~p>", [self()]) ++ " \"report5 string\"\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {info_report,
			       ignored,
			       {self(), std_info, "report5 string"}},
			      #state{}) end)).
handle_event09_test() ->
    ?assertEqual({{ok, #state{}},
		  str_format("INFO <~p>", [self()]) ++ " type \"report6 string\"\n"},
		 io_helper(
		   fun() -> custom_error_report:handle_event(
			      {info_report,
			       ignored,
			       {self(), type, "report6 string"}},
			      #state{}) end)).
handle_event10_test() ->
    ?assertEqual({{ok, #state{}},
		  ""},
		 io_helper(
		   fun() -> custom_error_report:handle_event(ignored, #state{}) end)).

%%--------------------------------------------------------------------
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
handle_call_test() ->
    ?assertMatch({ok, ok, #state{}}, custom_error_report:handle_call(ignored, #state{})).

%%--------------------------------------------------------------------
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
handle_info_test() ->
    ?assertMatch({ok, #state{}}, custom_error_report:handle_info(ignored, #state{})).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @end
%%--------------------------------------------------------------------
terminate_test() -> 
    ?assertMatch(ok, custom_error_report:terminate(ignored1, #state{})).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
code_change_test() ->
    ?assertMatch({ok, #state{}}, custom_error_report:code_change(ignored1, #state{}, ignored2)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
