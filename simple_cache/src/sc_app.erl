%%% @author Logan, Merritt and Carlsson, "Erlang and OTP in Action", Manning Publications
%%% @copyright (C) 2011, Manning Publications
%%% @doc
%%% simple_cache application
%%%
%%% - transcribed and modified by Stephen P. Schaefer
%%%
%%% @end
%%% Created :  6 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_app).

-behaviour(application).

-export([start/2,stop/1]).

%% @doc application callback: invokes sc_sup:start_link/0
-spec start(atom(), list()) -> tuple(ok, pid())|tuple(error, any()).
start(_StartType, _StartArgs) ->
    case sc_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

%% @doc application callback: do nothing, return ok.
-spec stop(any()) -> ok.
stop(_state) ->
    ok.
