%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
-module(atask_gen_server).

%% API
-export([call/2, message/2, reply_async/4]).
-export([bindl/4]).
-export([wait_reply/2, wait_reply/3, wait_reply/4, wait_reply/5,
         handle_reply/3, pure_handle_reply/3]).
-export([state/1]).

%%%===================================================================
%%% API
%%%===================================================================

call(Process, Request) ->
    atask:call(Process, '$gen_call', Request).

message({PId, MRef}, Message) ->
    PId ! {message, MRef, Message}.

bind(Callback, Async, NextAsyncFun, Offset, State) ->
    atask_gen_server:wait_reply(
      Async,
      fun({ok, Val}) ->
              atask_gen_server:wait_reply(
                NextAsyncFun(),
                Callback({ok, Val})
               );
         ({error, Reason})->
              Callback({error, Reason}),
              State
      end, Offset, State).


bindl(Callback, _Async, [], Acc) ->
    Callback(ok, ok, Acc),
    fun(_Offset, State) ->
            State
    end;
bindl(Callback, Async, [Arg|T], Acc) ->
    atask_gen_server:wait_reply(
      Async(Arg), 
      fun({ok, Val}) ->
              NAcc = Callback(Arg, {ok, Val}, Acc),
              bindl(Async, Callback, T, NAcc);
         ({error, Reason}) ->
              Callback(Arg, {error, Reason}, Acc)
      end).

return(Fun, Offset, State) ->
    Fun(Offset, State).

reply_async(MRef, From, Offset, State) ->
    wait_reply(
      fun(Reply, S) ->
              gen_server:reply(From, Reply),
              S
      end, MRef, Offset, State).

wait_reply(MRef, Callback) ->
    wait_reply(MRef, Callback, infinity).

wait_reply(MRef, Callback, Timeout) ->
    fun(Offset, State) ->
            wait_reply(MRef, Callback, Offset, State, Timeout)
    end.

wait_reply(MRef, Callback, Offset, State) when is_reference(MRef), is_function(Callback) ->
    wait_reply(Callback, MRef, Offset, State, infinity);

wait_reply(Callback, MRef, Offset, State) when is_reference(MRef), is_function(Callback) ->
    wait_reply(Callback, MRef, Offset, State, infinity).

wait_reply(Callback, {ok, MRef}, Offset, State, Timeout) when is_reference(MRef) ->
    wait_reply(Callback, MRef, Offset, State, Timeout);
wait_reply(MRef, Callback, Offset, State, Timeout)
  when is_reference(MRef), is_function(Callback) ->
    wait_reply(Callback, MRef, Offset, State, Timeout);
wait_reply(Callback, MRef, Offset, State, Timeout) 
  when is_reference(MRef), is_function(Callback) ->
    NCallback = atask:wait_reply(Callback, MRef, Timeout),
    Callbacks = element(Offset, State),
    NCallbacks = store(MRef, NCallback, Callbacks),
    setelement(Offset, State, NCallbacks);
wait_reply(Callback, Reply, Offset, State, _Timeout) ->
    execute_callback(Callback, Reply, Offset, State).

handle_reply(Reply, Offset, State) ->
    StateRec = element(1, State),
    NState = pure_handle_reply(Reply, Offset, State),
    case element(1, NState) of
        StateRec ->
            {noreply, NState};
        _ ->
            NState
    end.

pure_handle_reply({message, MRef, Message}, Offset, State) when is_reference(MRef) ->
    Callbacks = element(Offset, State),
    case find(MRef, Callbacks) of
        {ok, Callback} ->
            execute_callback(Callback, {message, Message}, Offset, State);
        error ->
            State
    end;

pure_handle_reply({MRef, Reply}, Offset, State) when is_reference(MRef) ->
    do_pure_handle_reply({MRef, Reply}, Offset, State);

pure_handle_reply({'DOWN', MRef, Type, Object, Reason}, Offset, State) when is_reference(MRef) ->
    do_pure_handle_reply({'DOWN', MRef, Type, Object, Reason}, Offset, State).

state(Process) when is_atom(Process) ->
    case whereis(Process) of
        undefined ->
            undefined;
        PId ->
            state(PId)
    end;
state(PId) when is_pid(PId) ->
    element(2, lists:nth(1, element(2, lists:nth(3, lists:nth(5, element(4, sys:get_status(PId))))))).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_pure_handle_reply(Reply, Offset, State) ->
    F = fun(MRef) ->
                Callbacks = element(Offset, State),
                case find(MRef, Callbacks) of
                    {ok, Callback} ->
                        NCallbacks = erase(MRef, Callbacks),
                        {ok, {Callback, setelement(Offset, State, NCallbacks)}};
                    error ->
                        {error, State}
                end
        end,
    atask:handle_reply(Reply, F).


store(Key, Value, Dict) when is_list(Dict) ->
    orddict:store(Key, Value, Dict);
store(Key, Value, Dict) ->
    dict:store(Key, Value, Dict).

find(Key, Dict) when is_list(Dict) ->
    orddict:find(Key, Dict);
find(Key, Dict) ->
    dict:find(Key, Dict).

erase(Key, Dict) when is_list(Dict) ->
    orddict:erase(Key, Dict);
erase(Key, Dict) ->
    dict:erase(Key, Dict).

execute_callback(Callback, Reply, Offset, State) ->
    case erlang:fun_info(Callback) of
        {arity, 1} ->
            case Callback(Reply) of
                NCallback when is_function(NCallback) ->
                    NCallback(Offset, State);
                _Other ->
                    State   
            end;
        {arity, 2} ->
            Callback(Reply, State);
        {arity, N} ->
            exit({invalid_callback, Callback, N})
    end.
            
