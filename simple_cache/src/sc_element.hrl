-include("eunit-2.1.7/include/eunit.hrl").

-record(state, {value, lease_time, start_time}).

%%%================================================================
%%% Types
%%%================================================================

-type child() :: pid() | undefined.

-type startchild_ret() :: {ok, Child :: child()}
                 | {ok, Child :: child(), Info :: term()}
                 | {error, startchild_err()}.

-type startchild_err() :: already_present
                 | {already_started, Child :: child()}
                 | term().

-type strategy() :: one_for_all
           | one_for_one
           | rest_for_one
           | simple_one_for_one.

-type restart() :: permanent | transient | temporary.

-type shutdown() :: brutal_kill | timeout().

-type worker() :: worker | supervisor.

-type startlink_ret() :: {ok, pid()}
                | ignore
                | {error, startlink_err()}.

-type startlink_err() :: {already_started, pid()} | shutdown | term().

-type time_period() :: integer() | infinity.

-type element_state() :: { term(), integer(), time_period() }.
