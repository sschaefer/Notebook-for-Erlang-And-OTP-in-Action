%%%------------------------------------------------------------------------------
%%% @author Martin and Eric <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc Application behavior for the RPC over TCP server.
%%%
%%%      - transcribed and modified for testing by Stephen P. Schaefer
%%% @end
%%%------------------------------------------------------------------------------
-module(tr_app).

-behavior(application).

-export([
	 start/2,
	 stop/1
	]).

%% @doc application callback: invokes tr_sup:start_link/0.
-spec start(atom(), list()) -> tuple(ok, pid())|tuple(error, any()).
start(_Type, _StartArgs) ->
    case tr_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

%% @doc application callback: do nothing, return ok.
-spec stop(any()) -> ok.
stop(_State) ->
    ok.
