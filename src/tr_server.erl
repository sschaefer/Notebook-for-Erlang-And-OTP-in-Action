%%%------------------------------------------------------------------------------
%%% @author Martin and Eric <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc RPC over TPC server.  This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%%      - transcribed and modified for testing by Stephen P. Schaer
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

%% for testing
% I wish the ifdef worked, but it doesn't
% -ifdef(EUNIT).
-export([
	 do_rpc/2,
	 split_out_mfa/1,
	 args_to_terms/1
	]).
% -endif.

-define(SERVER, ?MODULE).

-include("tr_server.hrl").

%%%==============================================================================
%%% API
%%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%%  where Pid = pid()
%% @end
%%------------------------------------------------------------------------------

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @spec start_link() -> {ok, Pid}
%% @doc Calls `start_link(Port)' with the default port

start_link() ->
    start_link(?DEFAULT_PORT).

%%------------------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server
%%
%% @spec get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%------------------------------------------------------------------------------

get_count() ->
    gen_server:call(?SERVER, get_count).

%%------------------------------------------------------------------------------
%% @doc Stops the server
%%
%% @spec stop() -> ok
%% @end
%%------------------------------------------------------------------------------

stop() ->
    gen_server:cast(?SERVER, stop).

%%%==============================================================================
%%% genserver callbacks
%%%==============================================================================

%% @spec init(_) -> {ok, #state{}}
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

%% @spec handle_call(_Request, _From, #state{}) -> {reply, ok, State}
handle_call(Request, _From, State) ->
    case Request of
	get_count ->
	    {reply, {ok, State#state.request_count}, State};
	_ ->
	    {reply, ok, State}
    end.

%% @spec handle_cast/(_Msg, State)  -> {noreply, State}
handle_cast(Msg, State) ->
    case Msg of
	stop ->
	    {stop, normal, State};
	_ ->
	    {noreply, State}
    end.

%% @spec handle_info({tcp, Socket, RawData}, State) -> {noreply, State}
%% @doc  accepts a connection to the service TCP socket
handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
     {ok, _Sock} = gen_tcp:accept(LSock),
     {noreply, State};
%% @spec handle_info(_Info, State) -> {noreply, State}
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(_Resason, _State) -> ok
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(_OldVsn, State, _Extra) -> {ok, State}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------------
%% @spec do_rpc(Socket, RawData) -> ok
do_rpc(Socket, RawData) ->
    try
	{M, F, A} = split_out_mfa(RawData),
	Result = apply(M, F, A),
	gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
	_Class:Err ->
	    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

%% @spec split_out_mfa(string()) ->
%%       {Module = string(), Function = string(), Arguments = [string()]}
split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [M, F, A]} =
	 re:run(MFA,
		"(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
		[{capture, [1, 2, 3], list}, ungreedy]),
    {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

%% @spec args_to_terms(list()) -> list()
%% @doc convert a representation of arguments into a list of terms to be used
%%      with apply/1
args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.

