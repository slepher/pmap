%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(result_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).

-export_type([result_t/4]).

%% API
-export([new/1, '>>='/3, return/2, fail/2, lift/2]).
-export([ask/1, get/1, put/2]).
-export([exec/4]).


%% Type constructors in erlang is not supported, I clould not implement type of async_t by other monad_t
%% TODO: expand it
-opaque result_t(_C, _S, _M, _A) :: any().

%%%===================================================f================
%%% API
%%%===================================================================

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec '>>='(result_t(C, S, M, A), fun( (A) -> result_t(C, S,  M, B) ), M) -> result_t(C, S, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    Monad = real(M),
    Monad:'>>='(X, Fun).

-spec return(A, M) -> result_t(_C, _S, M, A).
return(A, {?MODULE, M}) ->
    Monad = real(M),
    Monad:return(A).

-spec fail(any(), M) -> result_t(_C, _S, M, _A).
fail(X, {?MODULE, M}) ->
    Monad = real(M),
    Monad:fail(X).

-spec lift(monad:monadic(M, A), M) -> result_t(_C, _S,  M, A).
lift(F, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M2:lift(M1:lift(F)).

-spec ask(M) -> result_t(_C, _S, M, _A).
ask({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M2:lift(M1:ask()).

-spec get(M) -> result_t(S, S,  M, _A).
get({?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M2:get().

-spec put(S, M) -> result_t(ok, S, M, _A).
put(State, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    M2:put(State).

-spec exec(result_t(C, S, M, _A), S, C, M) -> S.
exec(X, State, CallbacksGS, {?MODULE, M}) ->
    M1 = reader_t:new(M),
    M2 = state_t:new(M1),
    (M2:exec(X, State))(CallbacksGS).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
real(M) ->
    state_t:new(reader_t:new(M)).
