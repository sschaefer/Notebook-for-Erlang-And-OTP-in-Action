%%%-------------------------------------------------------------------
%%% @author  Logan, Merritt, and Carlsson
%%% @copyright (C) 2012, 
%%% @doc
%%% - transcribed and modified (heavily) by Stephen P. Schaefer
%%% @end
%%% Created : 28 Jan 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(sc_sup).

-behaviour(supervisor).

-include("sup.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% API: Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% supervisor callback: Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, {sup_flags(), [child_spec()]}}.
init([]) ->
    Sup_name = sc_element_sup,
    Sup_module = sc_element_sup,
    Sup_callback_module = [sc_element_sup],
    Sup_restart = permanent,
    Sup_shutdown = 2000,
    Sup_type = supervisor,
    ElementSup = {Sup_name, {Sup_module, start_link, []},
		  Sup_restart, Sup_shutdown, Sup_type, Sup_callback_module},
    Mgr_name = sc_event,
    Mgr_module = sc_event,
    Mgr_callback_module = dynamic,
    Mgr_restart = permanent,
    Mgr_shutdown = 2000,
    Mgr_type = worker,
    ElementManager = {Mgr_name, {Mgr_module, start_link, []},
		      Mgr_restart, Mgr_shutdown, Mgr_type, Mgr_callback_module},
    Children = [ElementSup, ElementManager],

    RestartStrategy = one_for_one,
    MaxRestarts = 4,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
