%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
-module(async_t_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-compile({parse_transform, do}).

-record(state, {callbacks = maps:new(), acc0 = [], acc = []}).

-include_lib("eunit/include/eunit.hrl").
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
    {ok, PId} = echo_server:start(),
    [{echo_server, PId}|Config].

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
    [test_async_t, test_chain_async, test_chain_async_fail, test_async_t_with_message, test_async_t_with_message_handler].

%% Test cases starts here.
%%--------------------------------------------------------------------

test_async_t() ->
    [{doc, "Describe the main purpose of this test case"}].
test_async_t(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity_m),
    MRef = echo_server:echo(EchoServer, {ok, hello}),
    M0 = Monad:promise(MRef),
    Reply = Monad:wait(M0, infinity),
    ?assertEqual({ok, hello}, Reply).

test_chain_async() ->
    [{doc, "Describe the main purpose of this test case"}].
test_chain_async(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity_m),
    MRef = echo_server:echo(EchoServer, hello),
    M0 = do([Monad || 
                R1 <- Monad:promise(MRef),
                R2 <- Monad:promise(echo_server:echo(EchoServer, {ok, world})),
                return({R1, R2})
               ]),
    Reply = Monad:wait(M0),
    ?assertEqual({ok, {hello, world}}, Reply).


test_chain_async_fail() ->
    [{doc, "test fail in async_t"}].
test_chain_async_fail(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity_m),
    MRef = echo_server:echo(EchoServer, {ok, hello}),
    M0 = do([Monad || 
                   R1 <- Monad:promise(MRef),
                   R2 <- Monad:promise(echo_server:echo(EchoServer, {error, world})),
                   R3 <- Monad:promise(echo_server:echo(EchoServer, hello)),
                   return({R1, R2, R3})
               ]),
    Reply = Monad:wait(M0),
    ?assertEqual({error, world}, Reply).

test_async_t_with_message() ->
    [{doc, "test async_t with message"}].

test_async_t_with_message(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity_m),
    MRef = echo_server:echo_with_messages(EchoServer, [message, message], hello),
    M0 = do([Monad || 
                   R1 <- Monad:promise(MRef),
                   R2 <- Monad:promise(echo_server:echo_with_messages(EchoServer, [message, message, message], world)),
                   return({R1, R2})
               ]),
    Reply = Monad:wait(M0,
                       fun({ok, R}, #state{acc = Acc}) ->
                               {R, Acc};
                          ({message, Message}, #state{acc = Acc} = State)->
                               NAcc = [Message|Acc],
                               State#state{acc = NAcc}
                       end, #state.callbacks, #state{}, infinity),
    ?assertEqual({{hello, world}, lists:duplicate(5, message)}, Reply).

test_async_t_with_message_handler() ->
    [{doc, "test async_t with message_handler"}].

test_async_t_with_message_handler(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity_m),
    MRef = echo_server:echo_with_messages(EchoServer, [message, message], hello),
    M0 = do([Monad || 
                R1 <- Monad:promise(MRef),
                R2 <- Monad:handle_message(
                           do([Monad ||
                                  Monad:promise(echo_server:echo_with_messages(
                                                  EchoServer, lists:duplicate(5, message), world)),
                                  Monad:promise(echo_server:echo_with_messages(
                                                  EchoServer, lists:duplicate(3, message), world))
                              ]),
                           fun(Message, #state{acc0 = Acc0} = State) ->
                                   State#state{acc0 = [Message|Acc0]}
                           end),
                R3 <- Monad:promise(
                           echo_server:echo_with_messages(
                             EchoServer, lists:duplicate(3, message), world)),
                return({R1, R2, R3})
               ]),
    Reply = Monad:wait(M0,
                       fun({ok, R}, #state{acc0 = Acc0, acc = Acc}) ->
                               {R, Acc0, Acc};
                          ({message, Message}, #state{acc = Acc} = State)->
                               NAcc = [Message|Acc],
                               State#state{acc = NAcc}
                       end, #state.callbacks, #state{}, infinity),
    ?assertEqual({{hello, world, world}, lists:duplicate(8, message), lists:duplicate(5, message)}, Reply).
