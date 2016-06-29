%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
%%% depricated
-module(atask_gen_server).

%% API
-export([call/2, message/2, reply_async/4]).
-export([bindl/4, ok_bind/0, ok_bind/1]).
-export([wait_reply/2, wait_reply/3, wait_reply/4, wait_reply/5,
         wait_call/3, wait_call/4,
         handle_reply/3, pure_handle_reply/3]).
-export([update_state/3, update_bind/2]).
-export([state/1]).

%%%===================================================================
%%% API
%%%===================================================================

call(Process, Request) ->
    atask:call(Process, '$gen_call', Request).

message({PId, MRef}, Message) ->
    PId ! {message, MRef, Message}.

ok_bind() ->
    F = fun(_Offset, State) ->
                State
        end,
    F.

ok_bind(Fun) ->
    fun(Offset, State) ->
            case Fun(State) of
                Bind when is_function(Bind) ->
                    Bind(Offset, State);
                NState ->
                    NState
            end
    end.

update_bind(Fun, Bind) ->
    fun(Offset, State) ->
            NState = Fun(State),
            Bind(Offset, NState)
    end.

update_state(Bind, Offset, State) when is_function(Bind) ->
    Bind(Offset, State);
update_state(NState, _Offset, State) when is_tuple(NState) ->
    case element(1, NState) of
        state ->
            NState;
        _ ->
            State
    end;
update_state(_Other, _Offset, State) ->
    State.

bindl(Callback, _Async, [], Acc) ->
    Callback(ok, ok, Acc),
    fun(_Offset, State) ->
            State
    end;
bindl(Callback, Async, [Arg|T], Acc) ->
    MRef =
        case erlang:fun_info(Async, arity) of
            {arrity, 1} ->
                {ok, Async(Arg)};
            {arity, 2} ->
                {ok, Async(Arg, Acc)};
            {arity, N} ->
                {error, {invalid_async_fun, N}}
        end,
    atask_gen_server:wait_reply(
      MRef,
      fun({ok, Val}) ->
              NAcc = Callback(Arg, {ok, Val}, Acc),
              case NAcc of
                  stop ->
                      ok;
                  NAcc ->
                      bindl(Callback, Async, T, NAcc)
              end;
         ({message, Message}) ->
              Callback(Arg, {message, Message}, Acc);
         ({error, Reason}) ->
              Callback(Arg, {error, Reason}, Acc)
      end).

reply_async(MRef, From, Offset, State) ->
    wait_reply(
      fun(Reply, S) ->
              gen_server:reply(From, Reply),
              S
      end, MRef, Offset, State).

wait_call(Fun, Process, Message) ->
    wait_reply(call(Process, Message), Fun).

wait_call(Fun, Process, Message, Timeout) ->
    wait_reply(call(Process, Message), Fun, Timeout).

wait_reply(MRef, Callback) ->
    wait_reply(MRef, Callback, infinity).

wait_reply(MRef, Callback, Timeout) ->
    fun(Offset, State) ->
            wait_reply(MRef, Callback, Offset, State, Timeout)
    end.

wait_reply(MRef, Callback, Offset, State) ->
    wait_reply(MRef, Callback, Offset, State, infinity).

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
wait_reply(Reply, Callback, Offset, State, _Timeout) when is_function(Callback) ->
    execute_callback(Callback, Reply, Offset, State);
wait_reply(Callback, Reply, Offset, State, _Timeout) when is_function(Callback) ->
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
    erlang:demonitor(MRef, [flush]),
    Callbacks = element(Offset, State),
    case find(MRef, Callbacks) of
        {ok, Callback} ->
            NCallbacks = erase(MRef, Callbacks),
            NState = setelement(Offset, State, NCallbacks),
            execute_callback(Callback, Reply, Offset, NState);
        error ->
            State
    end;

pure_handle_reply({'DOWN', MRef, _, _, Reason}, Offset, State) when is_reference(MRef) ->
    pure_handle_reply({MRef, {error, {no_such_process, Reason}}}, Offset, State).

state(Process) when is_atom(Process) ->
    case whereis(Process) of
        undefined ->
            undefined;
        PId ->
            state(PId)
    end;
state(PId) when is_pid(PId) ->
    Status = sys:get_status(PId),
    element(2, lists:nth(1, element(2, lists:nth(3, lists:nth(5, element(4, Status)))))).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
store(Key, Value, Dict) when is_map(Dict) ->
    maps:put(Key, Value, Dict);
store(Key, Value, Dict) when is_list(Dict) ->
    orddict:store(Key, Value, Dict);
store(Key, Value, Dict) ->
    dict:store(Key, Value, Dict).

find(Key, Dict) when is_map(Dict) ->
    maps:find(Key, Dict);
find(Key, Dict) when is_list(Dict) ->
    orddict:find(Key, Dict);
find(Key, Dict) ->
    dict:find(Key, Dict).

erase(Key, Dict) when is_map(Dict) ->
    maps:remove(Key, Dict);
erase(Key, Dict) when is_list(Dict) ->
    orddict:erase(Key, Dict);
erase(Key, Dict) ->
    dict:erase(Key, Dict).

execute_callback(Callback, Reply, Offset, State) ->
    case erlang:fun_info(Callback, arity) of
        {arity, 1} ->
            case Callback(Reply) of
                NCallback when is_function(NCallback) ->
                    NCallback(Offset, State);
                _Other ->
                    State
            end;
        {arity, 2} ->
            case Callback(Reply, State) of
                NCallback when is_function(NCallback) ->
                    NCallback(Offset, State);
                NState ->
                    NState
            end;
        {arity, N} ->
            exit({invalid_callback, Callback, N})
    end.
