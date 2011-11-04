%%%------------------------------------------------------------------------------
%%% @author Stephen P. Schaefer <sschaefer@acm.org>
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Eunit tests for minimal genserver behavior implementation provided in
%%%      the "OTP in Action" book by Logan, Merritt and Carlsson.
%%%
%%%      To test a different genserver behavior implementation, one nees to
%%%      change the value of MODULE_TESTED, and the name of the include file.
%%%      A change in the eunit test version must be accomodated in the other
%%%      include file name.
%%% @end
%%%==============================================================================
-module(minimal_genserver_test).

-include("eunit-2.1.7/include/eunit.hrl").

-define(MODULE_TESTED, minimal_genserver).
-include("minimal_genserver.hrl").

-export([]).
% The export triggers EDoc documentation.
-ifndef(TEST).
-export([
	 init_test/0,
	 handle_call_test/0,
	 handle_cast_test/0,
	 handle_info_test/0,
	 terminate_test/0,
	 code_change_test/0
	]).
-endif.

%%%==============================================================================
%%% Tests
%%%==============================================================================

%% @doc test of init/1
-spec init_test() -> tuple(ok, #state{}).
init_test() ->
    {ok, #state{}} = ?MODULE_TESTED:init([]).

%% @doc test of handle_call/3
-spec handle_call_test() -> tuple(reply, ok, preserved_state).
handle_call_test() ->
    {reply, ok, preserved_state} = ?MODULE_TESTED:handle_call(discarded_request, discarded_from, preserved_state).

%% @doc test of handle_cast/2
-spec handle_cast_test() -> {noreply, preserved_state}.
handle_cast_test() ->
    {noreply, preserved_state} = ?MODULE_TESTED:handle_cast(discarded_msg, preserved_state).

%% @doc test of handle_info/2
-spec handle_info_test() -> {noreply, preserved_state}.
handle_info_test() ->
    {noreply, preserved_state} = ?MODULE_TESTED:handle_info(discarded_info, preserved_state).

%% @doc test of terminate/2
-spec terminate_test() -> ok.
terminate_test() ->
    ok = ?MODULE_TESTED:terminate(discarded_reason, discarded_state).

%% @doc test of code_change/3
-spec code_change_test() -> {ok, preserved_state}.
code_change_test() ->
    {ok, preserved_state} = ?MODULE_TESTED:code_change(old_version, preserved_state, extra).
