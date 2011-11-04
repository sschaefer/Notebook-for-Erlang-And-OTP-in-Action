%%%------------------------------------------------------------------------------
%%% @author Stephen P. Schaefer <sschaefer@acm.org>
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Eunit tests for tr_server provided in
%%%      the "OTP in Action" book by Logan, Merritt and Carlsson.
%%%
%%% Setup
%%%      To test a different genserver behavior implementation, one nees to
%%%      change the value of MODULE_TESTED, and the name of the include file.
%%%      A change in the eunit test version must be accomodated in the other
%%%      include file name.
%%% @end
%%%------------------------------------------------------------------------------
-module(tr_server_test).

-define(MODULE_TESTED, tr_server).
-export([]).

-include("tr_server.hrl").

%%%==============================================================================
%%% Tests
%%%==============================================================================

-define(TEST_PORT0, 65534).
-define(TEST_PORT1, 65533).
-define(TEST_PORT2, 65532).
-define(TEST_PORT3, 65531).
-define(TEST_PORT4, 65530).
-define(TEST_PORT5, 65529).

%% @doc test of init/1
%%      init/1 must work before start_link/1 can work
-spec init_test() -> tuple(ok, #state{}).
init_test() ->
    {ok, State, Count} = ?MODULE_TESTED:init([?TEST_PORT0]),
    ?assert(State#state.port =:= ?TEST_PORT0),
    Count = 0,
    ok = gen_tcp:close(State#state.lsock).

%%      handle_cast(stop, _) must work before stop/0 can work
-spec handle_cast_test() -> tuple(noreply, preserved_state).
handle_cast_test() ->
    {stop, normal, preserved_state} =
	?MODULE_TESTED:handle_cast(stop, preserved_state),
    {noreply, preserved_state} =
	?MODULE_TESTED:handle_cast(discarded_msg, preserved_state).

%% @doc test of stop/0
%%      stop() must work so we can clean up after start_link/1 test
-spec stop_test() -> ok.
stop_test() ->
    ok = ?MODULE_TESTED:stop().
    
%% @doc test of start_link/1
-spec start_link_1_test() -> ok.
start_link_1_test() ->
    {ok, _Pid} = ?MODULE_TESTED:start_link(?TEST_PORT1),
    {ok, Socket} =
	gen_tcp:connect(
	  "localhost",
	  ?TEST_PORT1,
	  [binary, {packet, 0}]),
    ok = gen_tcp:close(Socket),
    ok = ?MODULE_TESTED:stop().

%% @doc test of start_link/0
-spec start_link_0_test() -> ok.
start_link_0_test() ->
    {ok, _Pid} = ?MODULE_TESTED:start_link(),
    {ok, Socket} = gen_tcp:connect(
		     "localhost",
		     ?DEFAULT_PORT,
		     [binary, {packet, 0}]),
    ok = gen_tcp:close(Socket),
    ok = ?MODULE_TESTED:stop().

%% @doc test of handle_call/3
%%      handle_call(get_count, _, State) must work before get_count() can work
-spec handle_call_test() -> tuple(reply, ok, preserved_state).
-define(FAKE_COUNT, 1234).
handle_call_test() ->
    {reply, {ok, ?FAKE_COUNT}, #state{request_count = ?FAKE_COUNT}} =
	?MODULE_TESTED:handle_call(
	   get_count,
	   discarded_from,
	   #state{request_count = ?FAKE_COUNT}),
    {reply, ok, preserved_state} =
	?MODULE_TESTED:handle_call(
	   discarded_request,
	   discarded_from,
	   preserved_state).

%% @doc test of get_count
-spec get_count_test() -> ok.
get_count_test() ->
    {ok, _Pid} = ?MODULE_TESTED:start_link(?TEST_PORT2),
% start_link/1 provokes the timeout message to handle_info, which waits until
% it gets a connection, queueing up any other callback until it finishes
% the following gets past that
    {ok, ClientSock} =
	gen_tcp:connect(
	  "localhost",
	  ?TEST_PORT2,
	  [binary, {packet, 0}, {active, false}]),
    {ok, 0} = ?MODULE_TESTED:get_count(),
    ok = gen_tcp:close(ClientSock),
    ok = ?MODULE_TESTED:stop().

%% @doc test of handle_info/2
-spec do_null_rpc_client(integer()) -> ok.
do_null_rpc_client(Port) ->
    ok = timer:sleep(500),
    {ok, ClientSock} =
	gen_tcp:connect(
	  "localhost",
	  Port,
	  [binary, {packet, 0}, {active, false}]),
    ok = gen_tcp:close(ClientSock).
    
-spec handle_info_test() -> tuple(noreply, preserved_state).
handle_info_test() ->
% test of data received on a TCP connection
    {ok, LSock} = gen_tcp:listen(?TEST_PORT4, [{active, false}]),
    Pid = spawn(fun() -> do_rpc_client(?TEST_PORT4) end),
    {ok, ServerSock} = gen_tcp:accept(LSock),
    Pid ! self(),
    {ok, RawData} = gen_tcp:recv(ServerSock, 0),
    {noreply, #state{request_count = 1}} =
	?MODULE_TESTED:handle_info(
	   {tcp, ServerSock, RawData},
	   #state{lsock = LSock, request_count = 0}),
    receive
	{ok, _Receive} -> ok
    end,
    ok = gen_tcp:close(ServerSock),
    ok = gen_tcp:close(LSock),
% test of setting up the listening connection
    {ok, LSock2} = gen_tcp:listen(?TEST_PORT5, [{active, true}]),
    % handle_info(timeout, ...) doesn't return until socket gets connection
    _Pid2 = spawn(fun() -> do_null_rpc_client(?TEST_PORT5) end),
    {noreply, #state{lsock = LSock2, request_count = 0}} =
	?MODULE_TESTED:handle_info(
	   timeout,
	   #state{lsock = LSock2}),
    ok = gen_tcp:close(LSock2),
% test of arbitrary other out-of-band message
    {noreply, preserved_state} =
	?MODULE_TESTED:handle_info(discarded_info, preserved_state).

%% @doc test of terminate/2
-spec terminate_test() -> ok.
terminate_test() ->
    ok = ?MODULE_TESTED:terminate(discarded_reason, discarded_state).

%% @doc test of code_change/3
-spec code_change_test() -> tuple(ok, preserved_state).
code_change_test() ->
    {ok, preserved_state} =
	?MODULE_TESTED:code_change(old_version, preserved_state, extra).
