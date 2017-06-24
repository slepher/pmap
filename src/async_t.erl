%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).

-export_type([async_t/5]).

%% API
-export([new/1, '>>='/3, return/2, fail/2, lift/2, lift_mr/2]).
-export([get/1, put/2, find_ref/2, get_ref/3, put_ref/3, remove_ref/2, 
         get_acc/1, put_acc/2, callCC/2]).
-export([lift_reply/2, lift_reply_all/2, pure_return/2, message/2, hijack/2, pass/1]).
-export([promise/2, promise/3, then/3, pmap/2, par/2]).
-export([wait/2, wait/3, wait/4, wait/5, wait/6]).
-export([run/5, wait_receive/4, handle_message/3, handle_info/4]).


%% Type constructors in erlang is not supported, I clould not implement type of async_t by other monad_t
%% TODO: expand it
-opaque async_t(_C, _S, _R, _M, _A) :: any().

%%%===================================================================
%%% API
%%%===================================================================

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec '>>='(async_t(C, S, R, M, A), fun( (A) -> async_t(C, S, R, M, B) ), M) -> async_t(C, S, R, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(X, Fun).

-spec return(A, M) -> async_t(_C, _S, _R, M, A).
return(A, {?MODULE, M}) ->
    Monad = real(M),
    Monad:return(A).

-spec fail(any(), M) -> async_t(_C, _S, _R, M, _A).
fail(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:fail(X).

-spec lift(monad:monadic(M, A), M) -> async_t(_C, _S, _R, M, A).
lift(F, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:lift(F)).

lift_mr(MonadR, {?MODULE, M}) ->
    MR = async_r_t:new(M),
    M1 = cont_t:new(MR),
    M2 = reply_t:new(M1),
    M2:lift(M1:lift(MonadR)).

-spec get(M) -> async_t(S, S, _R, M, _A).
get({?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:get()).

-spec put(S, M) -> async_t(ok, S, _R, M, _A).
put(State, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:put(State)).

-spec get_acc(M) -> async_t(S, S, _R, M, _A).
get_acc({?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:get_acc()).

-spec put_acc(S, M) -> async_t(ok, S, _R, M, _A).
put_acc(Acc, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:put_acc(Acc)).


find_ref(MRef, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:find_ref(MRef)).

get_ref(MRef, Default, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:get_ref(MRef, Default)).

put_ref(MRef, Value, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:put_ref(MRef, Value)).

remove_ref(MRef, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:lift_mr(MR:remove_ref(MRef)).

lift_reply_all(F, {?MODULE, M}) ->
    Monad = real(M),
    Monad:lift(F).

-spec lift_reply(async_t(C, S, R, M, A), M) -> async_t(C, S, R, M, reply_t:reply_t(identity_m, A)).
lift_reply(F, {?MODULE, M}) ->
    Monad = real(M),
    Monad:lift_reply(F).

pure_return(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:pure_return(X).

message(A, {?MODULE, _M} = Monad) ->
    Monad:pure_return({message, A}).

-spec callCC(fun((fun( (A) -> async_t(C, S, R, M, _B) ))-> async_t(C, S, R, M, A)), M) -> async_t(C, S, R, M, A).
callCC(F,  {?MODULE, M}) ->
    MR = async_r_t:new(M),
    M1 = cont_t:new(MR),
    M2 = reply_t:new(M1),
    M2:lift(M1:callCC(F)).

-spec promise(any(), M) -> async_t(_C, _S, _R, M, _A).
promise(MRef, {?MODULE, _M} = Monad) ->
    promise(MRef, infinity, Monad).

-spec promise(any(), integer(), M) -> async_t(_C, _S, _R, M, _A).
promise(Action, Timeout, {?MODULE, M} = Monad) when is_function(Action, 0)->
    MR = async_r_t:new(M),
    fun(K) ->
            MRef = Action(),
            do([MR || 
                   AccRef <- MR:get_acc_ref(),
                   begin 
                       NK = callback_with_timeout(K, MRef, Timeout, Monad),
                       MR:put_ref(MRef, {NK, AccRef})
                   end
               ])
    end;
promise(MRef, Timeout, {?MODULE, _M} = Monad) when is_reference(MRef) ->
    Monad:promise(fun() -> MRef end, Timeout).

-spec then(async_t(C, S, R, M, A), fun((A) -> async_t(C, S, R, M, B)), M) -> async_t(C, S, R, M, B).
then(X, Then, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(Monad:lift(X), Then).

%% TODO: usage of Acc here is not right
pmap(Promises, {?MODULE, _M} = Monad) when is_list(Promises) ->
    NPromises = maps:from_list(lists:zip(lists:seq(1, length(Promises)), Promises)),
    do([Monad || 
           Value <- Monad:pmap(NPromises),
           Monad:pure_return(maps:values(Value))
       ]);
pmap(Promises, {?MODULE, _M} = Monad) when is_map(Promises) ->
    Ref = make_ref(),
    NPromises = 
        maps:map(
          fun(Key, Promise) ->
                  do([Monad ||
                         {Working, Completed} <- Monad:get_ref(Ref, {[], maps:new()}),
                         Monad:put_ref(Ref, {[Key|Working], Completed}),
                         Val <- Monad:lift_reply(Promise),
                         {NWorking, NCompleted} <- Monad:get_ref(Ref, {[], maps:new()}),
                         begin 
                             NNWorking = lists:delete(Key, NWorking),
                             NNCompleted = maps:put(Key, Val, NCompleted),
                             Monad:par([Monad:pure_return({message, {Key, Val}}),
                                        case NNWorking of
                                            [] ->
                                                do([Monad ||
                                                       Monad:remove_ref(Ref),
                                                       Monad:pure_return(NNCompleted)
                                                   ]);
                                            _ ->
                                                do([Monad ||
                                                       Monad:put_ref(
                                                         Ref, {NNWorking, NNCompleted}),
                                                       Monad:pass()
                                                   ])
                                        end])
                         end
                     ])
          end, Promises),
    Monad:par(maps:values(NPromises)).

par(Promises, {?MODULE, M}) ->
    MR = async_r_t:new(M),
    fun(K) ->
            monad:sequence(MR, lists:map(fun(Promise) -> Promise(K) end, Promises))
    end.            

handle_message(X, MessageHandler, {?MODULE, M} = Monad) ->
    NMessageHandler = callback_to_k(MessageHandler, {?MODULE, M}),
    do([Monad ||
           Value <- Monad:lift_reply_all(X),
           case Value of
               {message, Message} ->
                   Monad:hijack(NMessageHandler(Message));
               Reply ->
                   Monad:pure_return(Reply)
           end
       ]).

hijack(MR, {?MODULE, _M}) ->
    fun(_K) ->
            MR
    end.

pass({?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    Monad:hijack(MR:return(ok)).

run(X, Callback, Offset, State, {?MODULE, M} = Monad) ->
    MR = async_r_t:new(M),
    K = callback_to_k(Callback, Monad),
    CallbacksGS = state_callbacks_gs(Offset),
    Ref = make_ref(),
    MR:exec(X(K), CallbacksGS, Ref, State).

wait(X, {?MODULE, _M} = Monad) ->
    wait(X, infinity, Monad).

wait(X, Callback, {?MODULE, _M} = Monad) when is_function(Callback) ->
    wait(X, Callback, infinity, Monad);
wait(X, Timeout, {?MODULE, _M} = Monad) ->
    wait(X, 2, {state, maps:new()}, Timeout, Monad).

wait(X, Callback, Timeout, {?MODULE, _M} = Monad) ->
    wait(X, Callback, 2, {state, maps:new()}, Timeout, Monad).

wait(X, Offset, State, Timeout, {?MODULE, M} = Monad) ->
    wait(X,
         fun({message,__M}, S) ->
                 S;
            (A, _State) ->
                 M:return(A)
         end, Offset, State, Timeout, Monad).

wait(X, Callback, Offset, State, Timeout, {?MODULE, _M} = Monad) ->
    NState = run(X, Callback, Offset, State, Monad),
    case same_type_state(NState, State) of
        true ->
            wait_receive(Offset, NState, Timeout, Monad);
        false ->
            NState
    end.

wait_receive(Offset, State, Timeout, {?MODULE, M} = Monad) ->
    {CallbacksG, _CallbacksS} = state_callbacks_gs(Offset),
    Callbacks = CallbacksG(State),
    case maps:size(Callbacks) of
        0 -> 
            M:return(State);
        _ ->
            receive 
                Info ->
                    case handle_info(Info, Offset, State, Monad) of
                        unhandled ->
                            wait_receive(Offset, State, Timeout, Monad);
                        MNState ->
                            do([M ||
                                   NState <- MNState,
                                   case same_type_state(NState, State) of
                                       true ->
                                           wait_receive(Offset, NState, Timeout, Monad);
                                       false ->
                                           M:return(NState)
                                   end
                               ])
                    end
            after Timeout ->
                    lists:foldl(
                      fun(MRef, MS) ->
                              do([M || 
                                     S <- MS,
                                     case handle_info({MRef, {error, timeout}}, Offset, S, Monad) of
                                         unhandled ->
                                             M:return(S);
                                         NS ->
                                             NS
                                     end
                                 ])
                      end, State, maps:keys(Callbacks))
            end
    end.
                    
handle_info(Info, Offset, State, {?MODULE, M}) ->
    MR = async_r_t:new(M),
    {CallbacksG, CallbacksS} = state_callbacks_gs(Offset),
    Callbacks = CallbacksG(State),
    case info_to_a(Info) of
        {MRef, A} ->
            case handle_a(MRef, A, Callbacks) of
                {Callback, AccRef, NCallbacks} ->
                    NState = CallbacksS(NCallbacks, State),
                    MR:exec(Callback(A), {CallbacksG, CallbacksS}, AccRef, NState);
                error ->
                    M:return(State)
            end;
        unhandled ->
            unhandled
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
real(M) ->
    reply_t:new(cont_t:new(async_r_t:new(M))).

callback_to_k(Callback, {?MODULE, M}) when is_function(Callback, 0) ->
    MR = async_r_t:new(M),
    fun(_A) ->
            case Callback() of
                NMonadMR when is_function(NMonadMR) ->
                    NMonadMR;
                _ ->
                    MR:return(ok)
            end
    end;
callback_to_k(Callback, {?MODULE, M}) when is_function(Callback, 1) ->
    MR = async_r_t:new(M),
    fun(A) ->
            case Callback(A) of
                NMonadMR when is_function(NMonadMR) ->
                    NMonadMR;
                _ ->
                    MR:return(ok)
            end
    end;
callback_to_k(Callback, {?MODULE, M}) when is_function(Callback, 2) ->
    MR = async_r_t:new(M),
    fun(A) ->
            do([MR || 
                   State <- MR:get(),
                   NState <- MR:lift(Callback(A, State)),
                   MR:put(NState)
               ])
    end;
callback_to_k(Callback, {?MODULE, M}) ->
    MR = async_r_t:new(M),
    MR:fail({invalid_callback, Callback}).

info_to_a({message, MRef, Message}) when is_reference(MRef) ->
    {MRef, {message, Message}};
info_to_a({MRef, Reply}) when is_reference(MRef) ->
    {MRef, Reply};
info_to_a({'DOWN', MRef, _, _, Reason}) when is_reference(MRef) ->
    {MRef, {error, {process_down, Reason}}};
info_to_a(_Info) ->
    unhandled.

handle_a(MRef, {message, _Message}, Callbacks) when is_reference(MRef) ->
    case maps:find(MRef, Callbacks) of
        {ok, {Callback, Acc}} ->
            {Callback, Acc, Callbacks};
        error ->
            error
    end;
handle_a(MRef, _Reply, Callbacks) when is_reference(MRef) ->
    erlang:demonitor(MRef, [flush]),
    case maps:find(MRef, Callbacks) of
        {ok, {Callback, Acc}} ->
            NCallbacks = maps:remove(MRef, Callbacks),
            {Callback, Acc, NCallbacks};
        error ->
            error
    end.

callback_with_timeout(Callback, MRef, Timeout, {?MODULE, _M}) when is_integer(Timeout) ->
    Timer = erlang:send_after(Timeout, self(), {MRef, {error, wait_timeout}}),
    fun(A) ->
            erlang:cancel_timer(Timer),
            Callback(A)
    end;
callback_with_timeout(Callback, _MRef, _Timeout, {?MODULE, _M}) ->
    Callback.

state_callbacks_gs(Offset) ->
    {fun(State) ->
             element(Offset, State)
     end,
     fun(Callbacks, State) ->
             setelement(Offset, State, Callbacks)
     end}.

same_type_state(NState, State) when is_tuple(NState), is_tuple(State) ->
    element(1, NState) == element(1, State);
same_type_state(_NState, _State) ->
    false.
