-define(DEFAULT_PORT, 1055).
-record(state, {port, lsock, request_count = 0}).
-type socket() :: _.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
%% do_rpc_client is a helper function for the testing.
-spec do_rpc_client(integer()) -> ok.
do_rpc_client(Port) ->
    ok = timer:sleep(500),
    {ok, ClientSock} =
	gen_tcp:connect(
	  "localhost",
	  Port,
	  [binary, {packet, 0}, {active, false}]),
    ok = gen_tcp:send(ClientSock, "lists:reverse([1,2,3]).\r\n"),
    receive
	Parent -> Parent ! gen_tcp:recv(ClientSock, 0)
    end,
    ok = gen_tcp:close(ClientSock).
-endif.
