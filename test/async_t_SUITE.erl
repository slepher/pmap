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
    [test_async_t, test_chain_async, test_chain_async_fail, test_async_t_with_message, test_async_t_with_message_handler, test_async_t_par,
     test_async_t_pmap, test_async_t_pmap_with_acc, test_local_acc_ref, test_async_t_local_acc_ref, test_cont_t_local].

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
                               [R, Acc];
                          ({message, Message}, #state{acc = Acc} = State)->
                               NAcc = [Message|Acc],
                               State#state{acc = NAcc}
                       end, #state.callbacks, #state{}, infinity),
    ?assertEqual([{hello, world}, lists:duplicate(5, message)], Reply).

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

test_async_t_par(_Config) ->
    MR = async_r_t:new(identity_m),
    Monad = async_t:new(identity_m),
    M1 = Monad:par(
           [Monad:message(hello_message),
            Monad:return(hello)
           ]),
    Reply = Monad:wait(M1,
              fun({message, M}) ->
                      io:format("message is ~p~n", [M]),
                      MR:put_acc(M);
                 (Reply) ->
                      do([MR ||
                             Acc <- MR:get_acc(),
                             MR:put({Acc, Reply})
                         ])
              end),
    ?assertEqual({hello_message, {ok, hello}}, Reply).
                        

test_async_t_pmap(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity_m),
    M0 = Monad:promise(fun() -> echo_server:echo(EchoServer, hello) end),
    Promises = lists:duplicate(3, M0),
    M1 = Monad:pmap(Promises),
    Reply = Monad:wait(M1),
    ?assertEqual([hello, hello, hello], Reply).
                               

test_async_t_pmap_with_acc(Config) ->
    EchoServer = proplists:get_value(echo_server, Config),
    Monad = async_t:new(identity_m),
    M0 =  Monad:promise(fun() -> echo_server:echo(EchoServer, hello) end),
    Promises = lists:foldl(
                 fun(N, Acc0) ->
                         MA = 
                             do([Monad || 
                                    Val <- Monad:lift_reply(M0),
                                    Acc <- Monad:get_acc(),
                                    Monad:put_acc([N|Acc]),
                                    Monad:pure_return(Val)
                                ]),
                         maps:put(N, MA, Acc0)
                 end, maps:new(), lists:seq(1, 3)),
    M1 = do([Monad ||
                Monad:put_acc([]),
                Monad:pmap(Promises)
            ]),
    MR = async_r_t:new(identity_m),
    Reply = Monad:wait(M1, 
              fun({message, _X}) -> 
                      MR:return(ok);
                 (X) ->
                      do([MR ||
                             Acc <- MR:get_acc(),
                             MR:put({X, Acc})
                         ])
              end
             ),
    ?assertEqual({maps:from_list([{1,  hello}, {2,  hello}, {3,  hello}]), [3,2,1]}, Reply).
                               
test_local_acc_ref(_Config) ->
    MR = async_r_t:new(identity_m),
    Ref0 = make_ref(),
    Ref1 = make_ref(),
    M0 = do([MR ||
                Ref <- MR:get_acc_ref(),
                MR:put(Ref)
            ]),
    M1 = MR:local_acc_ref(Ref1, M0),
    M2 = do([MR ||
                R1 <- MR:local_acc_ref(Ref1, MR:get_acc_ref()),
                R0 <- MR:get_acc_ref(),
                MR:put({R0, R1})
            ]),
    ?assertEqual(Ref0, MR:exec(M0, undefined, Ref0, undefined)),
    ?assertEqual(Ref1, MR:exec(M1, undefined, Ref0, undefined)),
    ?assertEqual({Ref0, Ref1}, MR:exec(M2, undefined, Ref0, undefined)).

test_async_t_local_acc_ref(_Config) ->
    Monad = async_t:new(identity_m),
    MR = async_r_t:new(identity_m),
    Ref = make_ref(),
    M0 = do([Monad ||
                Ref0 <- Monad:get_acc_ref(),
                Monad:pure_return(Ref0)
            ]),
    M1 = Monad:local_acc_ref(Ref, M0),
    M2 = do([Monad ||
                Ref0 <- M0,
                Ref1 <- M1,
                Ref2 <- Monad:get_acc_ref(),
                Monad:pure_return({Ref0, Ref1, Ref2})
            ]),
    {{R0, R1, R2}, R3} = Monad:wait(M2, fun(X) -> 
                                          do([MR || 
                                                 MRRef <- MR:get_acc_ref(),
                                                 MR:put({X, MRRef})])
                                  end),
    ?assertEqual(Ref, R1),
    ?assertEqual(R0, R2),
    ?assertEqual(R0, R3).

test_cont_t_local(_Config) ->
    MR = reader_t:new(identity_m),
    Monad = cont_t:new(MR),
    RefO = make_ref(),
    Ref = make_ref(),
    
    M0 = do([Monad ||
                Ref0 <- Monad:lift(MR:ask()),
                return(Ref0)
            ]),
    M1 = cont_local(Ref, M0),
    M2 = do([Monad ||
                Ref0 <- M0,
                Ref1 <- M1,
                Ref2 <- Monad:lift(MR:ask()),
                return({Ref0, Ref1, Ref2})
            ]),
    Reader = Monad:run(M2, fun(X) -> MR:return(X) end),
    {R0, R1, R2}= MR:run(Reader, RefO),
    ?assertEqual(RefO, R0),
    ?assertEqual(RefO, R2),
    ?assertEqual(Ref, R1).


cont_local(L, C) ->
    MR = reader_t:new(identity_m),
    fun(K) ->
            do([MR ||
                   R <- MR:ask(),
                   begin 
                       NK = 
                           fun(A) ->
                                   MR:local(fun(_) -> R end, K(A))
                           end,
                       MR:local(fun(_) -> L end, C(NK))
                   end
               ])
    end.
    
