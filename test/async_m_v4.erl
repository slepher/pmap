%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_m_v4).

-behaviour(monad).

%% API
-export(['>>='/2, return/1, fail/1]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API
%%%===================================================================
'>>='(X, Fun) ->
    then(X, Fun).

return(A) ->
    fun(Callback) -> Callback({ok, A}) end.

fail(R) ->
    fun(Callback) -> Callback({error, R}) end.

then(Promise, Then) ->
    fun(Callback) ->
            Promise(
              fun({error, Reason}) ->
                      Callback({error, Reason});
                 ({ok, Reply}) ->
                      NPromise = Then(Reply),
                      NPromise(fun(NReply) -> Callback(NReply) end);
                 (Reply) ->
                      NPromise = Then(Reply),
                      NPromise(fun(NReply) -> Callback(NReply) end)
              end)
    end.
