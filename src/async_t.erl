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

%% API
-export([new/1, '>>='/3, return/2, fail/2, lift/2]).
-export([ask/1, get/1, put/2, callCC/2]).
-export([promise/2, promise/3]).
-export([run/5, handle_info/4]).

%%%===================================================================
%%% API
%%%===================================================================

new(M) ->
    {?MODULE, M}.

real(M) ->
    reply_t:new(cont_t:new(state_t:new(reader_t:new(M)))).

'>>='(X, Fun, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(X, Fun).

return(A, {?MODULE, M}) ->
    Monad = real(M),
    Monad:return(A).

fail(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:fail(X).

lift(F, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = cont_t:new(M2),
    M4 = reply_t:new(M3),
    M4:lift(M3:lift(M2:lift(M1:lift(F)))).

ask({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = cont_t:new(M2),
    M4 = reply_t:new(M3),
    M4:lift(M3:lift(M2:lift(M1:ask()))).

get({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = cont_t:new(M2),
    M4 = reply_t:new(M3),
    M4:lift(M3:lift(M2:put())).

put(State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = cont_t:new(M2),
    M4 = reply_t:new(M3),
    M4:lift(M3:lift(M2:put(State))).

callCC(F,  {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = cont_t:new(M2),
    M4 = reply_t:new(M3),
    M4:lift(M3:callCC(F)).

promise(MRef, {?MODULE, _M} = Monad) ->
    promise(MRef, infinity, Monad).

promise(MRef, Timeout, {?MODULE, M} = Monad) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    fun(K) ->
            do([M2 || 
                   StoreCallback <- M2:lift(M1:ask()),
                   State <- M2:get(),
                   begin 
                       NK = callback_with_timeout(K, MRef, Timeout, Monad),
                       NState = StoreCallback(MRef, NK, State),
                       M2:put(NState)
                   end
               ])
    end.

run(X, Callback, Offset, State, {?MODULE, M} = Monad) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    K = callback_to_k(Callback, Monad),
    StoreCallback = state_store_callback(Offset),
    (M2:exec(X(K), State))(StoreCallback).

handle_info(Info, Offset, State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    StoreCallback = state_store_callback(Offset),
    Callbacks = element(Offset, State),
    case info_to_a(Info) of
        {MRef, A} ->
            case handle_a(MRef, A, Callbacks) of
                {Callback, NCallbacks} ->
                    NState = setelement(Offset, State, NCallbacks),
                    (M2:exec(Callback(A), NState))(StoreCallback);
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
callback_to_k(Callback, {?MODULE, M} = M4) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    fun(A) ->
            do([M2 || 
                   State <- M2:get(),
                   NState <- 
                       M2:lift(M1:lift(execute_callback(Callback, A, State, M4))),
                   M2:put(NState)
               ])
    end.

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
        {ok, Callback} ->
            {Callback, Callbacks};
        error ->
            error
    end;
handle_a(MRef, _Reply, Callbacks) when is_reference(MRef) ->
    erlang:demonitor(MRef, [flush]),
    case maps:find(MRef, Callbacks) of
        {ok, Callback} ->
            NCallbacks = maps:remove(MRef, Callbacks),
            {Callback, NCallbacks};
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

execute_callback(Callback, Value, State, {?MODULE, M}) when is_function(Callback) ->
    case erlang:fun_info(Callback, arity) of
        {arity, 0} ->
            Callback(),
            M:return(State);
        {arity, 1} ->
            Callback(Value),
            M:return(State);
        {arity, 2} ->
            Callback(Value, State);
        {arity, N} ->
            M:fail({invalid_callback, Callback, N})
    end;
execute_callback(Callback, _Value, _State, {?MODULE, M}) ->
    M:fail({invalid_callback, Callback}).

state_store_callback(Offset) ->
    fun(MRef, Callback, State) ->
            Callbacks = element(Offset, State),
            NCallbacks = maps:put(MRef, Callback, Callbacks),
            setelement(Offset, State, NCallbacks)
    end.

%% incorrect, tobe fixed
promise2(MRef, Timeout, {?MODULE, M} = Monad) ->
    Monad:callCC(
      fun(K) ->
              do([Monad ||
                     Callbacks <- Monad:get(),
                     begin 
                         NK = callback_with_timeout(K, MRef, Timeout, Monad),
                         Monad:put(maps:put(MRef, NK, Callbacks))
                     end,                    
                     A <- K(undefined),
                     NCallbacks <- Monad:get(),
                     case handle_a(MRef, A, NCallbacks) of
                         {Reply, Callback, NNCallbacks} ->
                             do([Monad ||
                                    Monad:put(NNCallbacks),
                                    Callback(Reply)
                                ]);
                         error ->
                             Monad:return(undefined)
                     end
                 ])
      end).
