%%% @author  <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, 
%%% @doc
%%% myio_tests - explore EUnit
%%% @end
%%% Created :  2 Dec 2011 by  <sps@thyrsus-laptop2>

-module(myio_tests).

-include("global.hrl").

greeter1_test() ->
    IO_capture = stdout_test:capture(),
    ok = myio:greeter("world"),
    stdout_test:release(IO_capture),
    Stdout = stdout_test:stdout(IO_capture),
    stdout_test:finish(IO_capture),
    ?assertEqual("hello, world.\n", Stdout).

greeter2_test() ->
    IO_capture = stdout_test:capture(),
    ok = myio:greeter("moon"),
    stdout_test:release(IO_capture),
    Stdout = stdout_test:stdout(IO_capture),
    stdout_test:finish(IO_capture),
    ?assertEqual("hello, moon.\n", Stdout).

greeter_test_() ->
    {foreach,
     % setup
     fun() -> stdout_test:capture() end,
     % cleanup
     fun(IO_capture) -> stdout_test:release(IO_capture),stdout_test:finish(IO_capture) end,
     % Instatiators
     [
      fun(IO_capture) -> ?_assertEqual({ok, "hello, world.\n"},
				       {myio:greeter("world"),
					stdout_test:stdout(IO_capture)}) end,
      fun(IO_capture) -> ?_assertEqual({ok, "hello, moon.\n"},
				       {myio:greeter("moon"),
					stdout_test:stdout(IO_capture)}) end
     ]}.

