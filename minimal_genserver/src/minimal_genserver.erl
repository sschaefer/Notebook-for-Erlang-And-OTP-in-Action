%%%------------------------------------------------------------------------------
%%% @author Martin & Eric <erlware-dev@googlegroups.com>
%%% [http://www.manning.com/logan]
%%% @author transcribed by Stephen P. Schaefer <sschaefer@acm.org>
%%% All errors may be assumed to have been introduced by Stephen P. Schaefer
%%%  [http://followingthesystemtutorial.blogspot.com]
%%% @copyright Stephen P. Schaefer
%%% @doc Transcribed from the minimal genserver behavior implementation code in
%%%      the "OTP in Action" book by Logan, Merritt and Carlsson.
%%% @end
%%%------------------------------------------------------------------------------
-module(minimal_genserver).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("minimal_genserver.hrl").

%%%==============================================================================
%%% genserver callbacks
%%%==============================================================================

%% @spec init(_Ignored) -> {ok, #state{}}
init(_) ->
    {ok, #state{}}.

%% @spec handle_call(_Request, _From, #state{}) -> {reply, ok, State}
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @spec handle_cast(_Msg, State)  -> {noreply, State}
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @spec handle_info(_Info, State) -> {noreply, State}
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(_Resason, _State) -> ok
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(_OldVsn, State, _Extra) -> {ok, State}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



