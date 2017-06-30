%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_r_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).

-export_type([async_r_t/3]).

%% API
-export([new/1, '>>='/3, return/2, fail/2, lift/2]).
-export([get/1, put/2]).
-export([get_acc_ref/1, local_acc_ref/3, get_acc/1, put_acc/2]).
-export([find_ref/2, get_ref/3, put_ref/3, remove_ref/2]).
-export([exec/5]).

-opaque async_r_t(S, M, A) :: fun((S) -> fun((reference()) -> fun((callback_gs(S)) -> monad:monadic(M, {S, A})))).
-type callback_gs(S) :: {fun((S) -> #{reference() => Val}), fun((#{reference() => Val}, S) -> S)}.

%%%===================================================f================
%%% API
%%%===================================================================

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec '>>='(async_r_t(S, M, A), fun( (A) -> async_r_t(S,  M, B) ), M) -> async_r_t(S, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(X, Fun).

-spec return(A, M) -> async_r_t(_S, M, A).
return(A, {?MODULE, M}) ->
    Monad = real(M),
    Monad:return(A).

-spec fail(any(), M) -> async_r_t(_S, M, _A).
fail(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:fail(X).

-spec lift(monad:monadic(M, A), M) -> async_r_t(_S,  M, A).
lift(F, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:lift(M1:lift(F))).

-spec get(M) -> async_r_t(S,  M, S).
get({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:get().

-spec put(S, M) -> async_r_t(S, M, ok).
put(State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:put(State).

-spec get_acc_ref(M) -> async_r_t(_S, M, reference()).
get_acc_ref({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:ask()).

-spec local_acc_ref(reference(), async_r_t(S, M, A), M) -> async_r_t(S, M, A).
local_acc_ref(Ref, X, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    fun(S) ->
            M2:local(fun(_) -> Ref end, X(S))
    end.

-spec get_acc(M) -> async_r_t(_S, M, _C).
get_acc({?MODULE, _M} = Monad) ->
    do([Monad || 
           Ref <- Monad:get_acc_ref(),
           Monad:get_ref(Ref, undefined)
       ]).

-spec put_acc(_C, M) -> async_r_t(_S, M, ok).
put_acc(Acc, {?MODULE, _M} = Monad) ->
    do([Monad || 
           Ref <- Monad:get_acc_ref(),
           Monad:put_ref(Ref, Acc)
       ]).

-spec find_ref(reference(), M) -> async_r_t(_S, M, {ok, _A} | error).
find_ref(MRef, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, _CallbacksSetter} <- ask(Monad),
           State <- Monad:get(),
           begin
               Callbacks = CallbacksGetter(State),
               return(maps:find(MRef, Callbacks))
           end
       ]).

-spec get_ref(reference(), A, M) -> async_r_t(_S, M, A).
get_ref(MRef, Default, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, _CallbacksSetter} <- ask(Monad),
           State <- Monad:get(),
           begin
               Callbacks = CallbacksGetter(State),
               return(maps:get(MRef, Callbacks, Default))
           end
       ]).

-spec put_ref(reference(), _A, M) -> async_r_t(_S, M, ok).
put_ref(MRef, Data, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- ask(Monad),
           State <- Monad:get(),
           begin
               Callbacks = CallbacksGetter(State),
               NCallbacks = maps:put(MRef, Data, Callbacks),
               NState = CallbacksSetter(NCallbacks, State),
               Monad:put(NState)
           end
       ]).

-spec remove_ref(reference(), M) -> async_r_t(_S, M, ok).
remove_ref(MRef, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- ask(Monad),
           State <- Monad:get(),
           begin
               Callbacks = CallbacksGetter(State),
               NCallbacks = maps:remove(MRef, Callbacks),
               NState = CallbacksSetter(NCallbacks, State),
               Monad:put(NState)
           end
       ]).

-spec exec(async_r_t(S, M, _A), callback_gs(S), _Acc, S, M) -> monad:monadic(M, S).
exec(X, CallbacksGS, Acc, State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M1:run((M2:run(M3:exec(X, State), Acc)), CallbacksGS).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
real(M) ->
    state_t:new(reader_t:new(reader_t:new(M))).

ask({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = reader_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:lift(M1:ask())).
