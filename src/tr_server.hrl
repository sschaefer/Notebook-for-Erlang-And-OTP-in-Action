-define(DEFAULT_PORT, 1055).
-record(state, {port, lsock, request_count = 0}).

-type socket() :: _.

