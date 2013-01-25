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
-export([wait_reply/4, wait_reply/5, handle_reply/3, pure_handle_reply/3]).
-export([state/1]).

%%%===================================================================
%%% API
%%%===================================================================

call(Process, Request) ->
    atask:call(Process, '$gen_call', Request).

message({PId, MRef}, Message) ->
    PId ! {message, MRef, Message}.

reply_async(MRef, From, Offset, State) ->
    wait_reply(
      fun(Reply, S) ->
              gen_server:reply(From, Reply),
              S
      end, MRef, Offset, State).

wait_reply(Callback, MRef, Offset, State) ->
    wait_reply(Callback, MRef, Offset, State, infinity).

wait_reply(Callback, MRef, Offset, State, Timeout) when is_reference(MRef) ->
    NCallback = atask:wait_reply(Callback, MRef, Timeout),
    Callbacks = element(Offset, State),
    NCallbacks = store(MRef, NCallback, Callbacks),
    setelement(Offset, State, NCallbacks);
wait_reply(Callback, Reply, _Offset, State, _Timeout) ->
    Callback(Reply, State).

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
            Callback({message, Message}, State);
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



