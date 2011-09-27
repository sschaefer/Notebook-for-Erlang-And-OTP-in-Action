%%%------------------------------------------------------------------------------
%%% @author Stephen P. Schaefer <sschaefer@acm.org>
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Eunit tests for minimal genserver behavior implementation provided in
%%%      the "OTP in Action" book by Logan, Merritt and Carlsson.
%%% @end
%%%------------------------------------------------------------------------------
-module(minimal_genserver_test).

%%%==============================================================================
%%% Setup
%%% @doc To test a different genserver behavior implementation, one nees to
%%%      change the value of MODULE_TESTED, and the name of the include file.
%%%      A change in the eunit test version must be accomodated in the other
%%%      include file name.
%%% @end
%%%==============================================================================

-include("eunit-2.1.7/include/eunit.hrl").

-define(MODULE_TESTED, minimal_genserver).
-include("minimal_genserver.hrl").

-export([]).

%%%==============================================================================
%%% Tests
%%%==============================================================================

%% @spec init_test() -> {ok, #state{}}
%% @end
%% @doc test of init/1
%% @end
init_test() ->
    {ok, #state{}} = ?MODULE_TESTED:init([]).

%% @spec handle_call_test -> {reply, ok, preserved_state}
%% @doc test of handle_call/3
handle_call_test() ->
    {reply, ok, preserved_state} = ?MODULE_TESTED:handle_call(discarded_request, discarded_from, preserved_state).

%% @spec handle_cast_test -> {noreply, preserved_state}
%% @doc test of handle_cast/2
handle_cast_test() ->
    {noreply, preserved_state} = ?MODULE_TESTED:handle_cast(discarded_msg, preserved_state).

%% @spec handle_info_test -> {noreply, preserved_state}
%% @doc test of handle_info/2
handle_info_test() ->
    {noreply, preserved_state} = ?MODULE_TESTED:handle_info(discarded_info, preserved_state).

%% @spec terminate_test -> ok
%% @doc test of terminate/2
terminate_test() ->
    ok = ?MODULE_TESTED:terminate(discarded_reason, discarded_state).

%% @spec code_change_test -> {ok, preserved_state}
%% @doc test of code_change/3
code_change_test() ->
    {ok, preserved_state} = ?MODULE_TESTED:code_change(old_version, preserved_state, extra).
