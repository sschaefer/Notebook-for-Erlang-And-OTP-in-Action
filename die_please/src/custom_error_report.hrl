-record(state, {}).

-include_lib("eunit/include/eunit.hrl").

-type gleader() :: pid().
-type format() :: string().
-type data() :: list().
-type error_type() :: any().
-type error_tag() :: term().
-type error_report() :: [{error_tag(), data()} | term()]
		| string()
		| term().
-type error_event() :: {error, gleader(), {pid(), format(), data()}} |
		 {error_report, gleader(), {pid(), std_error, error_report()}} |
		 {error_report, gleader(), {pid(), error_type(), error_report()}} |
		 {warning_msg, gleader(), {pid(), format(), data()}} |
		 {warning_report, gleader(), {pid(), std_warning, error_report()}} |
		 {warning_report, gleader(), {pid(), error_type(), error_report()}} |
		 {info_msg, gleader(), {pid(), format(), data()}} |
		 {info_report, gleader(), {pid(), std_info, error_report()}} |
		 {info_report, gleader(), {pid(), error_type(), error_report()}}.
