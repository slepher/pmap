%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
-module(pmap_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% Test server callback functions
%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    pmap:start(),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Initiation before each test case
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%%
%% Returns a list of all test cases in this test suite
%%
%% @spec all() -> TestCases
%% @end
%%--------------------------------------------------------------------
all() ->
    [test_pmap_task, test_pmap_monitor].

%% Test cases starts here.
%%--------------------------------------------------------------------
test_pmap_task() ->
    [{doc, "Describe the main purpose of this test case"}].

test_pmap_task(Config) when is_list(Config) ->
    Start = erlang:now(),
    V = 
    pmap:task(
      fun(N) ->
              pmap:atask(fun() -> timer:sleep(N * 100) end)
      end, lists:seq(1, 10), 2),
    End = erlang:now(),
    V = lists:reverse(lists:map(fun(N) -> {N, ok} end, lists:seq(1, 10))),
    Used = round(timer:now_diff(End, Start) / 100000),
    30 = Used.

test_pmap_monitor() ->
    [{doc, "Describe the main purpose of this test case"}].

test_pmap_monitor(Config) when is_list(Config) ->
    V = pmap:monitor_task(
          fun(N) ->
                  pmap:atask(fun() -> timer:sleep(N * 100) end)
          end, lists:seq(1, 10), 2),
    timer:sleep(150),
    {progress, {1, [3, 2]}} = pmap:status(V),
    timer:sleep(3000),
    {ok, FV} = pmap:status(V),
    FV = lists:reverse(lists:map(fun(N) -> {N, ok} end, lists:seq(1, 10))).

