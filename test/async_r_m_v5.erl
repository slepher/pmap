%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_r_m_v5).
-behaviour(monad).

%% API
-export(['>>='/2, return/1, fail/1, get/0, put/1, modify/1, ask/0, run/3]).

%%%===================================================================
%%% API
%%%===================================================================
'>>='(X, Fun) ->
    M = state_t:new(reader_t:new(identity_m)),
    M:'>>='(X, Fun).

return(A) ->
    M = state_t:new(reader_t:new(identity_m)),
    M:return(A).

fail(R) ->
    exit(R).

get() ->
    M = state_t:new(reader_t:new(identity_m)),
    M:get().

put(S) ->
    M = state_t:new(reader_t:new(identity_m)),
    M:put(S).

modify(S) ->
    M = state_t:new(reader_t:new(identity_m)),
    M:modify(S).

ask() ->
    R = reader_t:new(identity_m),
    M = state_t:new(R),
    M:lift(R:ask()).

run(AsyncRM, Offset, State) ->
    R = reader_t:new(identity_m),
    M = state_t:new(R),
    R:run(M:exec(AsyncRM, State), Offset).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
