%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Jul 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async).

-compile({parse_transform, do}).

%% API
-export([call/3, message/2, promise_action/2]).
-export([promise_mref/1, promise_mref/2]).

%%%===================================================================
%%% API
%%%===================================================================
%% Local or remote by pid
call(Pid, Label, Request) when is_pid(Pid) ->
    do_call(Pid, Label, Request);
%% Local by name
call(Name, Label, Request) when is_atom(Name) -> 
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            do_call(Pid, Label, Request);
        undefined ->
            {error, noproc}
    end;
%% Global by name
call({global, _Name}=Process, Label, Request) ->
    case where(Process) of
        Pid when is_pid(Pid) ->
           do_call(Pid, Label, Request);
        undefined ->
            {error, noproc}
    end;
%% Local by name in disguise
call({Name, Node}, Label, Request) when Node =:= node() ->
    call(Name, Label, Request);
%% Remote by name
call({Name, Node}=Process, Label, Request) when is_atom(Name), is_atom(Node) ->
    do_call(Process, Label, Request);
call(Name, _Label, _Request) ->
    {error, {invalid_process_name, Name}}.

message({PId, MRef}, Message) ->
    catch PId ! {message, MRef, Message}.

promise_action(Action, Timeout) ->
    fun(Callback, StoreCallback, State) ->
            case Action() of
                MRef when is_reference(MRef) ->
                    NCallback = async_m:callback_with_timeout(MRef, Callback, Timeout),
                    StoreCallback(MRef, NCallback, State);
                Other ->
                    async_m:execute_callback(Callback, Other, State)
            end
    end.

promise_mref(MRef) ->
    promise_mref(MRef, infinity).

promise_mref(MRef, Timeout) ->
    fun(Callback, StoreCallback, State) ->
            NCallback = async_m:callback_with_timeout(MRef, Callback, Timeout),
            StoreCallback(MRef, NCallback, State)
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
where({global, Name}) -> global:whereis_name(Name).

do_call(Process, Label, Request) ->
    Mref = erlang:monitor(process, Process),
    erlang:send(Process, {Label, {self(), Mref}, Request}, [noconnect]),
    Mref.
