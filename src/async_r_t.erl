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

-export_type([async_r_t/5]).

%% API
-export([new/1, '>>='/3, return/2, fail/2, lift/2]).
-export([ask/1, get/1, put/2]).
-export([get_acc/1, put_acc/2]).
-export([find_ref/2, put_ref/3, remove_ref/2]).
-export([exec/5]).


%% Type constructors in erlang is not supported, I clould not implement type of async_t by other monad_t
%% TODO: expand it
-opaque async_r_t(_R, _C, _S, _M, _A) :: any().

%%%===================================================f================
%%% API
%%%===================================================================

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec '>>='(async_r_t(R, C, S, M, A), fun( (A) -> async_r_t(R, C, S,  M, B) ), M) -> async_r_t(R, C, S, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(X, Fun).

-spec return(A, M) -> async_r_t(_R, _C, _S, M, A).
return(A, {?MODULE, M}) ->
    Monad = real(M),
    Monad:return(A).

-spec fail(any(), M) -> async_r_t(_R, _C, _S, M, _A).
fail(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:fail(X).

-spec lift(monad:monadic(M, A), M) -> async_r_t(_R, _C, _S,  M, A).
lift(F, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:lift(M1:lift(F))).

-spec ask(M) -> async_r_t(R, _C, _S, M, R).
ask({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:lift(M1:ask())).

-spec get(M) -> async_r_t(_R, _C, S,  M, S).
get({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = state_t:new(M2),
    M3:get().

-spec put(S, M) -> async_r_t(_R, _C, S, M, ok).
put(State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = state_t:new(M2),
    M3:put(State).

-spec get_acc(M) -> async_r_t(_R, C, _S, M, C).
get_acc({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:get()).

-spec put_acc(C, M) -> async_r_t(_R, C, _S, M, ok).
put_acc(State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = state_t:new(M2),
    M3:lift(M2:put(State)).

find_ref(MRef, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, _CallbacksSetter} <- Monad:ask(),
           State <- Monad:get(),
           begin
               Callbacks = CallbacksGetter(State),
               return(maps:find(MRef, Callbacks))
           end
       ]).

put_ref(MRef, Data, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- Monad:ask(),
           State <- Monad:get(),
           begin
               Callbacks = CallbacksGetter(State),
               NCallbacks = maps:put(MRef, Data, Callbacks),
               NState = CallbacksSetter(NCallbacks, State),
               Monad:put(NState)
           end
       ]).

remove_ref(MRef, {?MODULE, _M} = Monad) ->
    do([Monad ||
           {CallbacksGetter, CallbacksSetter} <- Monad:ask(),
           State <- Monad:get(),
           begin
               Callbacks = CallbacksGetter(State),
               NCallbacks = maps:remove(MRef, Callbacks),
               NState = CallbacksSetter(NCallbacks, State),
               Monad:put(NState)
           end
       ]).

-spec exec(async_r_t(R, C, S, M, _A), R, C, S, M) -> S.
exec(X, CallbacksGS, Acc, State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M3 = state_t:new(M2),
    M1:run((M2:eval(M3:exec(X, State), Acc)), CallbacksGS).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
real(M) ->
    state_t:new(state_t:new(reader_t:new(M))).
