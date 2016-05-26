%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------

-module(async_m).

-behaviour(monad).

-export(['>>='/2, return/1, fail/1]).
-export([wait/1, wait/2]).
-export([get_state/0, put_state/1, update_state/1]).
-export([exec/4, wait_with_timeout/3, execute_callback/3]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
'>>='(M, Fun) ->
    fun(Callback, StoreCallback, State) ->
            M(
              fun({error, Reason}, NState) ->
                      execute_callback(Callback, {error, Reason}, NState);
                 ({message, Message}, NState) ->
                      execute_callback(Callback, {message, Message}, NState);
                 (Reply, NState) ->
                      Val = 
                          case Reply of
                              {ok, R} ->
                                  R;
                              ok ->
                                  ok;
                              Other -> 
                                  Other
                          end,
                      (Fun(Val))(
                        fun(NReply, NNState) ->
                                execute_callback(Callback, NReply, NNState)
                        end, StoreCallback, NState)
              end, StoreCallback, State)
    end.

return(A) -> 
    fun(Callback, _StoreCallback, State) -> 
            execute_callback(Callback, A, State)
    end.
                       
fail(X) -> 
    fun(Callback, _StoreCallback, State) ->
            execute_callback(Callback, {error, X}, State)
    end.

wait(Async) ->
    wait(Async, infinity).

wait(Async, Timeout) ->
    fun(Callback, StoreCallback, State) ->
            NCallback = wait_with_timeout(Async, Callback, Timeout),
            StoreCallback(Async, NCallback, State)
    end.

get_state() ->
    fun(Callback, _StoreCallback, State) ->
            execute_callback(Callback, State, State)
    end.

put_state(State) ->
    fun(Callback, _StoreCallback, _State) ->
            execute_callback(Callback, ok, State)
    end.

update_state(Fun) ->
    fun(Callback, _StoreCallback, State) ->
            NState = Fun(State),
            execute_callback(Callback, ok, NState)
    end.

exec(M, Callback, StoreCallback, State) ->
    M(Callback, StoreCallback, State).

wait_with_timeout(_Async, Callback, infinity) ->
    Callback;
wait_with_timeout(MRef, Callback, Timeout) when is_integer(Timeout), is_reference(MRef) ->
    Timer = erlang:send_after(Timeout, self(), {MRef, {error, wait_timeout}}),
    fun(Reply, State) ->
            erlang:cancel_timer(Timer),
            execute_callback(Callback, Reply, State)
    end;
wait_with_timeout(_Async, Callback, _Timeout) ->
    Callback.

execute_callback(Callback, Value, State) when is_function(Callback) ->
    case erlang:fun_info(Callback, arity) of
        {arity, 0} ->
            Callback(),
            State;
        {arity, 1} ->
            Callback(Value),
            State;
        {arity, 2} ->
            Callback(Value, State);
        {arity, N} ->
            exit({invalid_callback, Callback, N})
    end;
execute_callback(Callback, _Value, _State) ->
    exit({invalid_callback, Callback}).
%%%===================================================================
%%% Internal functions
%%%===================================================================
