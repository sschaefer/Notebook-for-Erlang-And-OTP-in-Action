%%%-------------------------------------------------------------------
%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%% Tests of the resource_discovery server module.
%%% @end
%%% Created :  6 Apr 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(resource_discovery_tests).

-include("resource_discovery.hrl").

-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc system_test/0 demonstrates the casts required before a resource is
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link_test() -> {ok, pid()}.
start_link_test() ->
    {ok, _Pid} = resource_discovery:start_link().

-spec init_test() -> {ok, #state{}}.
init_test() ->
    {ok, #state{target_resource_types = List,
		local_resource_tuples = Dict1,
		found_resource_tuples = Dict2}} = resource_discovery:init([]),
    List = [],
    0 = dict:size(Dict1),
    0 = dict:size(Dict2).
				 
add_target_resource_type_test() ->
    ok = resource_discovery:add_target_resource_type(test_type1).

add_local_resource_test() ->
    ok = resource_discovery:add_local_resource(test_type2, instance2).

handle_cast_test() ->
    {noreply, #state{target_resource_types = [added_type]}} =
	resource_discovery:handle_cast(
	  {add_target_resource_type, added_type},
	  #state{target_resource_types = [],
		 local_resource_tuples = dict:new(),
		 found_resource_tuples = dict:new()}),
    {noreply, #state{local_resource_tuples = L_r_t}} =
	resource_discovery:handle_cast(
	  {add_local_resource, {type3, instance3}},
	  #state{target_resource_types = [],
		 local_resource_tuples = dict:new(),
		 found_resource_tuples = dict:new()}),
    [instance3] = dict:fetch(type3, L_r_t),
    {ok, State0 } = resource_discovery:init([]),
    {noreply, State0} =
	resource_discovery:handle_cast(trade_resources, State0),
    {noreply, State0} =
	resource_discovery:handle_cast(
	  {trade_resources, {node(), dict:new()}},
	  State0),
    {noreply, State0} = resource_discovery:handle_cast(
	  {trade_resources, {noreply, dict:new()}},
	  State0).

fetch_resources_test() ->
    error = resource_discovery:fetch_resources(missing_type).

handle_call_test() ->
    {reply, error, #state{target_resource_types = [],
			  local_resource_tuples = Dict1,
			  found_resource_tuples = Dict2}} =
	resource_discovery:handle_call({fetch_resources, missing_type},
				       self(),
				       #state{target_resource_types = [],
					      local_resource_tuples = dict:new(),
					      found_resource_tuples = dict:new()}),
    0 = dict:size(Dict1),
    0 = dict:size(Dict2),
    {reply, {ok, [val1]}, #state{target_resource_types = [],
			   local_resource_tuples = Dict3,
			   found_resource_tuples = Dict4}} =
	resource_discovery:handle_call({fetch_resources, found_type},
				      self(),
				      #state{target_resource_types = [],
					     local_resource_tuples = dict:new(),
					     found_resource_tuples = dict:store(found_type, [val1], dict:new())}),
    0 = dict:size(Dict3),
    1 = dict:size(Dict4).

trade_resources_test() ->
    ok = resource_discovery:trade_resources().

%%%===================================================================
%%% System test
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc system_test/0 demonstrates the casts required before a resource is
%% available to the fetch_resources/1 call.
%% <ul>
%% <li> add_local_resource must have occurred somewhere</li>
%% <li> add_target_resource_type must have occurred on this node</li>
%% <li> trade_resources must have happened</li>
%% </ul>
%% in that order.  And due to the "cast" nature of the
%% communications, it may require two fetches to get the mailbox of
%% messages to be processed.
%%
%% @end
%%--------------------------------------------------------------------
-spec system_test() -> {ok, [system_instance2]}.
system_test() ->
    ok = resource_discovery:add_local_resource(system_type1, system_instance1),
% server hasn't processed casts yet!
    error = resource_discovery:fetch_resources(system_type1),
% Expected: server has processed casts, but no trade_resources() and empty
% target_resource_types means empty found_resource_tuples
    error = resource_discovery:fetch_resources(system_type1),
    ok = resource_discovery:trade_resources(),
% server hasn't processed casts yet!
    error = resource_discovery:fetch_resources(system_type1),
% Expected: server has processed casts, but still empty target_resource_types
% means empty found_resource_tuples types
    error = resource_discovery:fetch_resources(system_type1),
% now we declare a type we want
    ok = resource_discovery:add_target_resource_type(system_type2),
% ...and say we have it
    ok = resource_discovery:add_local_resource(system_type2, system_instance2),
% server hasn't processed casts yet!
    error = resource_discovery:fetch_resources(system_type1),
% Expected: no trade_resources means empty found_resource_tuples
    error = resource_discovery:fetch_resources(system_type2),
    ok = resource_discovery:trade_resources(),
% server hasn't procesed casts yet!
    error = resource_discovery:fetch_resources(system_type2),
% now it works.
    {ok, [system_instance2]} = resource_discovery:fetch_resources(system_type2).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
