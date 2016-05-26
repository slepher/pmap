%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_gen_server).

%% API
-export([call/2, message/2]).
-export([execute_async_m/4, execute_async_m/5]).
-export([handle_reply/3]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
call(Process, Request) ->
    atask:call(Process, '$gen_call', Request).

message({PId, MRef}, Message) ->
    PId ! {message, MRef, Message}.

execute_async_m(Monad, Callback, Offset, State) ->
    execute_async_m(Monad, Callback, Offset, State, infinity).

execute_async_m(Monad, Callback, Offset, State, Timeout) ->
    Store = store_callback(Offset, Timeout),
    async_m:exec(Monad, Callback, Store, State).

handle_reply({message, MRef, Message}, Offset, State) when is_reference(MRef) ->
    Callbacks = element(Offset, State),
    case find(MRef, Callbacks) of
        {ok, Callback} ->
            async_m:execute_callback(Callback, {message, Message}, State);
        error ->
            State
    end;
handle_reply({MRef, Reply}, Offset, State) when is_reference(MRef) ->
    erlang:demonitor(MRef, [flush]),
    Callbacks = element(Offset, State),
    case find(MRef, Callbacks) of
        {ok, Callback} ->
            NCallbacks = erase(MRef, Callbacks),
            NState = setelement(Offset, State, NCallbacks),
            async_m:execute_callback(Callback, Reply, NState);
        error ->
            State
    end;
handle_reply({'DOWN', MRef, _, _, Reason}, Offset, State) when is_reference(MRef) ->
    handle_reply({MRef, {error, {process_down, Reason}}}, Offset, State);
handle_reply(_Info, _Offset, _State) ->
    unhandled.

%%%===================================================================
%%% Internal functions
%%%===================================================================
store_callback(Offset, Timeout) ->
    fun(Callback, Reply, State) ->
            wait_reply(Callback, Reply, Offset, State, Timeout)
    end.

wait_reply(Callback, MRef, Offset, State, Timeout)
  when is_reference(MRef), is_function(Callback) ->
    NCallback = async_m:wait_with_timeout(Callback, MRef, Timeout),
    Callbacks = element(Offset, State),
    NCallbacks = store(MRef, NCallback, Callbacks),
    setelement(Offset, State, NCallbacks);
wait_reply(Reply, Callback, _Offset, State, _Timeout) when is_function(Callback) ->
    async_m:execute_callback(Callback, Reply, State);
wait_reply(Callback, Reply, _Offset, State, _Timeout) when is_function(Callback) ->
    async_m:execute_callback(Callback, Reply, State).

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
