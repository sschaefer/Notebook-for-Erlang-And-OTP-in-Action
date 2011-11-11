%%% @author Stephen P. Schaefer <sps@thyrsus-laptop2>
%%% @copyright (C) 2011, Stephen P. Schaefer
%%% @doc
%%% Test routines for simple_cache:sc_element module
%%% @end
%%% Created : 10 Nov 2011 by Stephen P. Schaefer <sps@thyrsus-laptop2>

-module(sc_element_test).

-include("eunit-2.1.7/include/eunit.hrl").

-export([]).

start_link_test() ->
    bad1 = sc_element:start_link(bad2, bad3).

create2_test() ->
    bad4 = sc_element:create(bad5, bad6).

create1_test() ->
    bad7 = sc_element:create(bad8).

fetch_test() ->
    bad9 = sc_element:fetch(bad10).

replace_test() ->
    bad11 = sc_element:replace(bad12, bad13).

delete_test() ->
    bad14 = sc_element:delete(bad15).

init_test() ->
    ?assertMatch(bad16, sc_element:init(bad17)),
    ?assertMatch(bad16_1, sc_element:init([bad17_1, bad17_2])).

handle_call_test() ->
    ?assertMatch(bad18, sc_element:handle_call(bad19, bad20, bad21)),
    ?assertMatch(bad22, sc_element:handle_call(fetch, bad23, bad24)).

handle_cast_test() ->
    ?assertMatch(bad25, sc_element:handle_cast(bad26, bad27)),
    ?assertMatch(bad28, sc_element:handle_cast({replace, bad29}, bad30)),
    ?assertMatch(bad31, sc_element:handle_cast(delete, bad32)).

handle_info_test() ->
    ?assertMatch(bad33, sc_element:handle_info(bad34, bad35)),
    ?assertMatch(bad36, sc_element:handle_info(timeout, bad37)).

terminate_test() ->
    bad38 = sc_element:terminate(bad39, bad40).

code_change_test() ->
    bad41 = sc_element:code_change(bad42, bad43, bad44).

    
