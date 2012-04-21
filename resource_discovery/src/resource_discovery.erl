%%%-------------------------------------------------------------------
%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created :  6 Apr 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(resource_discovery).

-behaviour(gen_server).

-include("resource_discovery.hrl").

%% API
-export([
	 start_link/0,
	 add_target_resource_type/1,
	 add_local_resource/2,
	 fetch_resources/1,
	 trade_resources/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% API: Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% API: Adds a type of interest to the local node
%%
%% @end
%%--------------------------------------------------------------------
-spec add_target_resource_type(term()) -> ok.
add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

%%--------------------------------------------------------------------
%% @doc
%% API: Adds a resource availale from the local node
%%
%% @end
%%--------------------------------------------------------------------
-spec add_local_resource(term(), term()) -> ok.
add_local_resource(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

%%--------------------------------------------------------------------
%% @doc
%% API: Retrieves discovered resources of a specified type.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_resources(term()) -> {ok, list()} | error.
fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

%%--------------------------------------------------------------------
%% @doc
%% API: Initiates "I have, I want" communication with all nodes, including
%% the local node.
%%
%% @end
%%--------------------------------------------------------------------
-spec trade_resources() -> ok.
trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% gen_server callback: Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{target_resource_types = [],
	        local_resource_tuples = dict:new(),
	        found_resource_tuples = dict:new()}}.

%%--------------------------------------------------------------------
%% @doc
%% gen_server callback: Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call({fetch_resources, term()}, pid(), #state{}) ->
			 {reply, ({ok, list()} | error), #state{}}.
handle_call({fetch_resources, Type}, _From, State) ->
    % show_state(State),
    {reply, dict:find(Type, State#state.found_resource_tuples), State}.

%%--------------------------------------------------------------------
%% @doc
%% gen_server callback: Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: any, State :: #state{}) -> {noreply, #state{}}.
handle_cast({add_target_resource_type, Type}, State) ->
    TargetTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Instance}}, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
    {noreply, State#state{local_resource_tuples = NewResourceTuples}};
handle_cast(trade_resources, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    AllNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
	      gen_server:cast({?SERVER, Node},
			      {trade_resources, {node(), ResourceTuples}})
      end,
      AllNodes),
    {noreply, State};
handle_cast({trade_resources, {ReplyTo, Remotes}},
	    #state{local_resource_tuples = Locals,
		   target_resource_types = TargetTypes,
		   found_resource_tuples = OldFound} = State) ->
    FilteredRemotes = resources_for_types(TargetTypes, Remotes),
    NewFound = add_resources(FilteredRemotes, OldFound),
    case ReplyTo of
	noreply ->
	    ok;
	_ ->
	    gen_server:cast({?SERVER, ReplyTo},
			    {trade_resources, {noreply, Locals}})
    end,
    {noreply, State#state{found_resource_tuples = NewFound}}.

%%--------------------------------------------------------------------
%% @doc
%% gen_server callback: Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(_Info :: term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    % show_state(State),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec add_resource_test() -> {ok, list()}.
add_resource_test() ->
    Dict0 = add_resource(type1, val1, dict:new()),
    {ok, [val1]} = dict:find(type1, Dict0),
    Dict1 = add_resource(type2, val2, Dict0),
    {ok, [val2]} = dict:find(type2, Dict1),
    Dict2 = add_resource(type1, val3, Dict1),
    {ok, [val3, val1]} = dict:find(type1, Dict2),
    Dict3 = add_resource(type1, val1, Dict2),
    {ok, [val1, val3]} = dict:find(type1, Dict3).
    
-spec add_resource(term(), term(), dict()) -> dict().
add_resource(Type, Resource, ResourceTuples) ->
    case dict:find(Type, ResourceTuples) of
	{ok, ResourceList} ->
	    NewList = [Resource | lists:delete(Resource, ResourceList)],
	    dict:store(Type, NewList, ResourceTuples);
	error ->
	    dict:store(Type, [Resource], ResourceTuples)
    end.

-spec resources_for_types_test() -> [{foo, bar}].
resources_for_types_test() ->
    [] = resources_for_types([], dict:new()),
    [] = resources_for_types([foo], dict:new()),
    [{foo, bar}] = resources_for_types([foo], dict:store(foo, [bar], dict:new())).

-spec resources_for_types(term, dict()) -> list().
resources_for_types(Types, ResourceTuples) ->
    Fun =
	fun(Type, Acc) ->
	    case dict:find(Type, ResourceTuples) of
		{ok, List} ->
		    [{Type, Instance} || Instance <- List] ++ Acc;
		error ->
		    Acc
	    end
	end,
    lists:foldl(Fun, [], Types).

-spec add_resources_test() -> 1.
add_resources_test() ->
    Dict0 = dict:new(),
    Dict0 = add_resources([], Dict0),
    Dict1 = add_resources([{a, b}], Dict0),
    [b] = dict:fetch(a, Dict1),
    1 = dict:size(Dict1).

-spec add_resources(list({term(), term()}), dict()) -> dict().
add_resources([{Type, Resource}|T], ResourceTuples) ->
    add_resources(T, add_resource(Type, Resource, ResourceTuples));
add_resources([], ResourceTuples) ->
    ResourceTuples.

-ifdef(DEBUG).
-spec show_state(#state{}) -> ok.
show_state(#state{
	     target_resource_types = Wanted,
	     local_resource_tuples = Has,
	     found_resource_tuples = Found}) ->
    lists:foreach(
      fun(Term) -> io:format(user, "Wanted: ~p~n", [Term]) end,
      Wanted),
    lists:foreach(
      fun({Type0, Resource0}) ->
	      io:format(user, "Has: Type: ~p Resource: ~p~n",
			[Type0, Resource0]) end,
      dict:to_list(Has)),
    lists:foreach(
      fun({Type0, Resource0}) ->
	      io:format(user, "Found: Type: ~p Resource: ~p~n",
			[Type0, Resource0]) end,
      dict:to_list(Found)).
-endif.
