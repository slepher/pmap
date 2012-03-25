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
-export([call/2, message/2]).
-export([wait_reply/4, wait_reply/5, handle_reply/3]).
-export([state/1]).

%%%===================================================================
%%% API
%%%===================================================================

call(Process, Request) ->
    atask:call(Process, '$gen_call', Request).

message({PId, MRef}, Message) ->
    PId ! {message, MRef, Message}.

wait_reply(Callback, MRef, Offset, State) ->
    wait_reply(Callback, MRef, Offset, State, infinity).

wait_reply(Callback, MRef, Offset, State, Timeout) when is_reference(MRef) ->
    NCallback = atask:wait_reply(Callback, MRef, Timeout),
    Callbacks = element(Offset, State),
    NCallbacks = dict:store(MRef, NCallback, Callbacks),
    setelement(Offset, State, NCallbacks).

handle_reply({message, MRef, Message}, Offset, State) when is_reference(MRef) ->
    StateRec = element(1, State),
    Callbacks = element(Offset, State),
    NState = 
        case dict:find(MRef, Callbacks) of
            {ok, Callback} ->
                Callback({message, Message}, State);
            error ->
                State
        end,
    case element(1, NState) of
        StateRec ->
            {noreply, NState};
        _ ->
            NState
    end;
handle_reply({MRef, Reply}, Offset, State) when is_reference(MRef) ->
    do_handle_reply({MRef, Reply}, Offset, State);

handle_reply({'DOWN', MRef, Type, Object, Reason}, Offset, State) when is_reference(MRef) ->
    do_handle_reply({'DOWN', MRef, Type, Object, Reason}, Offset, State).

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
do_handle_reply(Reply, Offset, State) ->
    StateRec = element(1, State),
    F = fun(MRef) ->
                Callbacks = element(Offset, State),
                case dict:find(MRef, Callbacks) of
                    {ok, Callback} ->
                        NCallbacks = dict:erase(MRef, Callbacks),
                        {ok, {Callback, setelement(Offset, State, NCallbacks)}};
                    error ->
                        {error, {noreply, State}}
                end
        end,
    NState = atask:handle_reply(Reply, F),
    case element(1, NState) of
        StateRec ->
            {noreply, NState};
        _ ->
            NState
    end.
