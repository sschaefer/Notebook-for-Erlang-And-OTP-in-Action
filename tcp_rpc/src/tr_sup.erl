%%%------------------------------------------------------------------------------
%%% @author Martin and Eric <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc Supervisor behavior for the RPC over TCP server.
%%%
%%%      - transcribed and modified for testing by Stephen P. Schaefer
%%% @end
%%%------------------------------------------------------------------------------
-module(tr_sup).

-behavior(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type strategy() :: one_for_all
           | one_for_one
           | rest_for_one
           | simple_one_for_one.

-type child_spec() :: {Id :: child_id(),
     StartFunc :: mfargs(),
     Restart :: restart(),
     Shutdown :: shutdown(),
     Type :: worker(),
     Modules :: modules()}.

-type child_id() :: term().

-type mfargs() :: {M :: module(), F :: atom(), A :: [term()] | undefined}.

-type restart() :: permanent | transient | temporary.

-type shutdown() :: brutal_kill | timeout().

-type worker() :: worker | supervisor.

-type modules() :: [module()] | dynamic.

-type startlink_return() :: {ok, pid()}
                | ignore
                | {error, startlink_err()}.

-type startlink_err() :: {already_started, pid()} | shutdown | term().

%% @doc API: invoke supervisor start_link
%%
%% <ul>
%%          <li>Register Process: on local node with name tr_sup</li>
%%          <li>Module: tr_sup</li>
%%          <li>Arguments to init/1: []</li></ul>
-spec start_link() -> startlink_return().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc supervisor callback
%%
%% <ul>
%%      <li>Restart Strategy: one_for_one,<ul>
%%          <li>Max Restarts: 0 - while under development</li>
%%          <li>Within Interval: 1 (second)</li></ul></li>
%%      <li>Children: one server, described as<ul>
%%          <li>ID: tr_server</li>
%%          <li>Start: module tr_server, function start_link, args []</li>
%%          <li>Restart: permanent</li>
%%          <li>Shutdown: 2000 millisecond grace period</li>
%%          <li>Type: worker</li>
%%          <li>Modules depended on: tr_server</li></ul></li></ul>
-spec init([]) -> tuple(ok,tuple(strategy(),MaxRestarts::integer(),TimeInterval::integer()),list(child_spec())) | ignore.
init([]) ->
    Server = {tr_server,
	      {tr_server, start_link, []},
	      permanent,
	      2000,
	      worker,
	      [tr_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

