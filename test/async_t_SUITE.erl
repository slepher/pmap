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

-record(state, {callbacks = maps:new()}).

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
    [test_async_t, test_chain_async, test_chain_async_fail].

%% Test cases starts here.
%%--------------------------------------------------------------------

test_async_t() ->
    [{doc, "Describe the main purpose of this test case"}].
test_async_t(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    M = async_t:new(identity_m),
    MRef = echo_server:echo(EchoServer, {ok, hello}),
    Monad = M:promise(MRef),
    State = M:run(Monad, 
                    fun(Reply) ->
                            ?assertEqual(Reply, {ok, hello})
                    end, #state.callbacks, #state{}),
    receive 
        Info ->
            case M:handle_info(Info, #state.callbacks, State) of
                unhandled ->
                    State;
                NState ->
                    ?assertEqual(NState, #state{}),
                    NState
            end
    end.


test_chain_async() ->
    [{doc, "Describe the main purpose of this test case"}].
test_chain_async(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    M = async_t:new(identity_m),
    MRef = echo_server:echo(EchoServer, hello),
    Monad = do([M || 
                   R1 <- M:promise(MRef),
                   R2 <- M:promise(echo_server:echo(EchoServer, {ok, world})),
                   M:return({R1, R2})
               ]),
    State = M:run(Monad, 
                    fun(Reply) ->
                            ?assertEqual(Reply, {ok, {hello, world}})
                    end, #state.callbacks, #state{}),
    receive 
        Info ->
            case M:handle_info(Info, #state.callbacks, State) of
                unhandled ->
                    State;
                NState ->
                    NState
            end
    end.

test_chain_async_fail() ->
    [{doc, "test fail in async_t"}].
test_chain_async_fail(Config) when is_list(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    M = async_t:new(identity_m),
    MRef = echo_server:echo(EchoServer, {ok, hello}),
    Monad = do([M || 
                   R1 <- M:promise(MRef),
                   R2 <- M:promise(echo_server:echo(EchoServer, {error, world})),
                   R3 <- M:promise(echo_server:echo(EchoServer, hello)),
                   M:return({R1, R2, R3})
               ]),
    State = M:run(Monad, 
                    fun(Reply) ->
                            ?assertEqual(Reply, {error, world})
                    end, #state.callbacks, #state{}),
    receive 
        Info ->
            case M:handle_info(Info, #state.callbacks, State) of
                unhandled ->
                    State;
                NState ->
                    NState
            end
    end.

test_async_t_with_message() ->
    [{doc, "test async_t with message"}].

test_async_t_with_message(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    M = async_t:new(identity_m),
    MRef = echo_server:echo_with_messages(EchoServer, [message, message], hello),
    Monad = do([M || 
                   R1 <- M:promise(MRef),
                   R2 <- M:promise(echo_server:echo(EchoServer, {error, world})),
                   R3 <- M:promise(echo_server:echo(EchoServer, hello)),
                   M:return({R1, R2, R3})
               ]),
    State = M:run(Monad, 
                    fun({error, R}) ->
                            ?assertEqual(R, world);
                        ({message, M})->
                            ?assertEqual(M, message)
                    end, #state.callbacks, #state{}),
    receive 
        Info ->
            case M:handle_info(Info, #state.callbacks, State) of
                unhandled ->
                    State;
                NState ->
                    NState
            end
    end.
