%%%-------------------------------------------------------------------
%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2012, 
%%% @doc
%%% Provide support for testing data sent to standard out.
%%% @end
%%% Created : 26 Jan 2012 by  <sps@thyrsus-laptop2>
%%%-------------------------------------------------------------------
-module(stdout_test).

%% API
-export([capture/0,release/1,finish/1,stdout/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc

% we create an intermediate group leader which remembers the IO
% and also invokes the same IO on the former group leader.

%% @end
%%--------------------------------------------------------------------

-spec capture() -> {pid(), pid()}.
capture() ->
    G0 = group_leader(),
    Shim = self(),
    G1 = spawn(fun() -> capture_io_process(Shim, G0) end),
    group_leader(G1, self()),
    {G0, G1}.

-spec release({pid(), pid()}) -> true.
release({G0, _}) ->
    group_leader(G0, self()).

-spec finish({pid(), pid()}) -> stop.
finish({_, G1}) ->
    G1 ! stop.

-spec stdout({pid(), pid()}) -> {stdout, pid()} | list().
stdout({_, G1}) ->
    G1 ! {stdout, self()},
    receive
	{G1, Buf} ->
	    Buf
    end.

-spec capture_io_process(pid(), pid()) -> ok.
capture_io_process(Pid, G0) ->
    capture_io_loop(Pid, G0, infinity, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec capture_io_loop(pid(), pid(), integer(), list()) -> ok.
capture_io_loop(Pid, G0, Wait, Buf) ->
    
    receive
	{io_request, From, ReplyAs, Req} ->
	    % keep a copy of the io
	    Buf1 = io_request(From, ReplyAs, Req, Buf),
	    G0 ! {io_request, From, ReplyAs, Req},
	    capture_io_loop(Pid, G0, Wait, Buf1);
	stop ->
	    % don't wait any more
	    capture_io_loop(Pid, G0, 0, Buf);
	{stdout, From} ->
	    % report the contents of the stdout buffer
	    From ! {self(), lists:flatten(lists:reverse(Buf))},
	    capture_io_loop(Pid, G0, Wait, Buf);
	_ ->
	    % discard
	    capture_io_loop(Pid, G0, Wait, Buf)
    after Wait ->
	    ok
    end.

%%%%%%%%%%%%
%%%%%%%%%%%%
%%% COPIED FROM eunit_proc.erl
%%%%%%%%%%%%
%%%%%%%%%%%%

%% Implementation of buffering I/O for group leader processes. (Note that
%% each batch of characters is just pushed on the buffer, so it needs to
%% be reversed when it is flushed.)

io_request(From, ReplyAs, Req, Buf) ->
    {Reply, Buf1} = io_request(Req, Buf),
    io_reply(From, ReplyAs, Reply),
    Buf1.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

io_request({put_chars, Chars}, Buf) ->
    {ok, [Chars | Buf]};
io_request({put_chars, M, F, As}, Buf) ->
    try apply(M, F, As) of
	Chars ->
	    {ok, [Chars | Buf]}
    catch
	C:T ->
	    {{error, {C,T,erlang:get_stacktrace()}}, Buf}
    end;
io_request({put_chars, _Enc, Chars}, Buf) ->
    io_request({put_chars, Chars}, Buf);
io_request({put_chars, _Enc, Mod, Func, Args}, Buf) ->
    io_request({put_chars, Mod, Func, Args}, Buf);
io_request({get_chars, _Enc, _Prompt, _N}, Buf) ->
    {eof, Buf};
io_request({get_chars, _Prompt, _N}, Buf) ->
    {eof, Buf};
io_request({get_line, _Prompt}, Buf) ->
    {eof, Buf};
io_request({get_line, _Enc, _Prompt}, Buf) ->
    {eof, Buf};
io_request({get_until, _Prompt, _M, _F, _As}, Buf) ->
    {eof, Buf};
io_request({setopts, _Opts}, Buf) ->
    {ok, Buf};
io_request(getopts, Buf) ->
    {error, {error, enotsup}, Buf};
io_request({get_geometry,columns}, Buf) ->
    {error, {error, enotsup}, Buf};
io_request({get_geometry,rows}, Buf) ->
    {error, {error, enotsup}, Buf};
io_request({requests, Reqs}, Buf) ->
    io_requests(Reqs, {ok, Buf});
io_request(_, Buf) ->
    {{error, request}, Buf}.

io_requests([R | Rs], {ok, Buf}) ->
    io_requests(Rs, io_request(R, Buf));
io_requests(_, Result) ->
    Result.
