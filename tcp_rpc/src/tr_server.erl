%%%------------------------------------------------------------------------------
%%% @author Martin and Eric <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc RPC over TCP server.  This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%%      
%%%      Internal functions exported so they may be exercised by tr_server_test.
%%%
%%%      - transcribed and modified for testing by Stephen P. Schaefer
%%% @end
%%%------------------------------------------------------------------------------
-module(tr_server).
-behaviour(gen_server).

%% API
-export([
	start_link/1,
	start_link/0,
	get_count/0,
	stop/0
       ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("tr_server.hrl").

%%%==============================================================================
%%% API
%%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc API: Starts the server
%% @end
%%------------------------------------------------------------------------------

-spec start_link(Port::integer()) -> tuple(ok, pid()).
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%------------------------------------------------------------------------------
%% @doc API: Calls `start_link(Port)' with the default port
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> tuple(ok, pid()).
start_link() ->
    start_link(?DEFAULT_PORT).

%%------------------------------------------------------------------------------
%% @doc API: Fetches the number of requests made to this server
%% @end
%%------------------------------------------------------------------------------
-spec get_count() -> tuple(ok, integer()).
get_count() ->
    gen_server:call(?SERVER, get_count).

%%------------------------------------------------------------------------------
%% @doc API: Stops the server
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

%%%==============================================================================
%%% gen_server callbacks
%%%==============================================================================

%% @doc gen_server callback: starts listening to a TCP Port,
%%     and returns the initial state.
-spec init(list(integer())) -> tuple(ok, #state{}).
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

%% @doc gen_server callback: provides a reply to get_count request, for anything else, noop
-spec handle_call(_Request, _From, #state{}) -> {reply, ok, #state{}}.
handle_call(Request, _From, State) ->
    case Request of
	get_count ->
	    {reply, {ok, State#state.request_count}, State};
	_ ->
	    {reply, ok, State}
    end.

%% @doc gen_server callback: provides {stop, normal, State} reply to stop message,
%%     {noreply, State} to others
-spec handle_cast(_Msg, #state{})  -> tuple(noreply, #state{}).
handle_cast(Msg, State) ->
    case Msg of
	stop ->
	    {stop, normal, State};
	_ ->
	    {noreply, State}
    end.

%% @doc gen_server callback: accepts a connection to the service TCP socket
-spec handle_info(tuple(atom(), socket(), string()) | any(), #state{}) ->
			 tuple(noreply, #state{}).
handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
     {ok, _Sock} = gen_tcp:accept(LSock),
     {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc gen_server callback: minimal implementation returns ok.
-spec terminate(_Resason, #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc gen_server callback: minimal implementation returns {ok, State}
-spec code_change(_OldVsn, #state{}, _Extra) -> tuple(ok, #state{}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------------

%% @doc Internal: Takes a socket on which to write and a string to parse and execute.
-spec do_rpc(socket(), string()) -> ok.
do_rpc(Socket, RawData) ->
    try
	{M, F, A} = split_out_mfa(RawData),
	Result = apply(M, F, A),
	gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
	_Class:Err ->
	    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

%% @doc Internal: parses a string into module, function and arguments
-spec split_out_mfa(string()) ->
       tuple(string(), string(), list(string())).
split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [M, F, A]} =
	 re:run(MFA,
		"(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
		[{capture, [1, 2, 3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

%% @doc Internal: convert a representation of arguments into a list of terms to be used
%%      with apply/1
-spec args_to_terms(list()) -> list().
args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

%%%==============================================================================
%%% Tests: unexported functions must be tested within the module.
%%%==============================================================================

-ifdef(TEST).
-define(TEST_PORT3, 65531).
%% @doc test the parsing of a string into a list of terms
%%     must work for split_out_mfa/1 to work
-spec args_to_terms_test() -> ok.
args_to_terms_test() ->
    [] =
	args_to_terms(""),
    [test_arg0] =
	args_to_terms("test_arg0"),
    [test_arg0, test_arg1] =
	args_to_terms("test_arg0, test_arg1").
    
%% @doc parse a string into the components needed to execute it as an
%%     Erlang function
%%     must work for do_rpc/2 to work
-spec split_out_mfa_test() -> tuple(atom(), atom(), list(atom())).
split_out_mfa_test() ->
    {test_module, test_function, [test_arg0, test_arg1]} =
	split_out_mfa(
	   "test_module:test_function(test_arg0, test_arg1).\r\n").

%% @doc test of do_rpc/2
%%      do_rpc/2 must work for handle_info/2 to work

-spec do_rpc_test() -> ok.
do_rpc_test() ->
    {ok, LSock} = gen_tcp:listen(?TEST_PORT3, [{active, false}]),
    Pid = spawn(fun() -> do_rpc_client(?TEST_PORT3) end),
    {ok, ServerSock} = gen_tcp:accept(LSock),
    Pid ! self(),
    Received = gen_tcp:recv(ServerSock, 0),
    {ok, RawData} = Received,
    ok = do_rpc(ServerSock, RawData),
    receive
	{ok, Receive} -> ok
    end,
    "[3,2,1]\n" = binary_to_list(Receive),
    ok = gen_tcp:close(ServerSock),
    ok = gen_tcp:close(LSock).

-endif.
