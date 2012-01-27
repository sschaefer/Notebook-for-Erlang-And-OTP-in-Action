%%% @author Logan, Merritt and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% Code illustrating use of SASL logging
%%% @end
%%% Created : 17 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(die_please).

-behavior(gen_server).

-include("die_please.hrl").

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SLEEP_TIME, (2*1000)).

%% API
%% @doc API: minimal startup function
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc gen_server callback: minimal initialization
-spec init([]) -> {ok, #state{}, integer()}.
init([]) ->
    {ok, #state{}, ?SLEEP_TIME}.

%% @doc gen_server callback: do nothing on any call.
-spec handle_call(term(), term(), #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @doc gen_server callback: do nothing on any cast.
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc gen_server callback: throw badmatch error on timeout
-spec handle_info(timeout, #state{}) -> {noreply, #state{}}.
handle_info(timeout, State) ->
    i_want_to_die = right_now,
    {noreply, State}.

%% @doc gen_server callback: do nothing on terminate
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc gen_server callback: do nothing on code change.
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State,  _Extra) ->
    {ok, State}.
