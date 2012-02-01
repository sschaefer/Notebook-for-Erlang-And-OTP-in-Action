%%% @author Logan, Merritt and Carlsson
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% Supervisor behavior for simple_cache of Erlang and OTP in Action
%%% @end
%%% Created : 10 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_element_sup).

-behavior(supervisor).

-include("sup.hrl").

-export([start_link/0,
	 start_child/2
	]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%================================================================
%%% API
%%%================================================================

%% @doc supervisor API implementation: invokes supervisor:start_link/3
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc supervisor API implementation: invokes supervisor:start_child/2
-spec start_child(term(), integer()) -> startchild_ret().
start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).

%%%================================================================
%%% Callbacks
%%%================================================================

%% @doc supervisor callback: return description of desired supervisor behavior
-spec init([]) -> {ok, {
       {strategy(), 0, 1},
       [{sc_element,
	 {sc_element,start_link, []},
	 restart(),
	 shutdown(),
	 worker(),
	 [sc_element]
	}]
      }}.
init([]) ->
    Element = {sc_element, {sc_element, start_link, []},
	       temporary, brutal_kill, worker, [sc_element]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

