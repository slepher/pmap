%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.local>
%%% @copyright (C) 2011, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2011 by Chen Slepher <slepher@larry.local>
%%%-------------------------------------------------------------------
-module(atask).

%% API
-export([start_and_action/3]).
-export([y0/1, y1/1, y2/1]).
-export([wait_reply/3, handle_reply/2]).
-export([call/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_and_action(StartFun, ActionFun, Args) ->
    case StartFun() of
        {ok, Server} ->
            apply(ActionFun, [Server|Args]);
        {error, Reason} ->
            {error, Reason}
    end.

y0(M) ->
    G = fun(F) -> M(fun() -> (F(F))() end) end,
    G(G).

y1(M) ->
    G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
    G(G).

y2(M) ->
    G = fun (F) -> M(fun(A, B) -> (F(F))(A, B) end) end,
    G(G).

wait_reply(Callback, MRef, infinity) when is_reference(MRef) ->
    Callback;
wait_reply(Callback, MRef, Timeout) when is_reference(MRef), is_integer(Timeout) ->
    Timer = erlang:send_after(Timeout, self(), {MRef, {error, timeout}}),
    fun(R, S) ->
            erlang:cancel_timer(Timer),
            Callback(R, S)
    end.

handle_reply({MRef, Reply}, UpdateState) when is_reference(MRef) ->
    erlang:demonitor(MRef, [flush]),
    case UpdateState(MRef) of
        {ok, {Callback, NState}} ->
            Callback(Reply, NState);
        {error, Response} ->
            Response
    end;

handle_reply({'DOWN', MRef, _, _, Reason}, UpdateState) when is_reference(MRef) ->
    handle_reply({MRef, {error, {no_such_process, Reason}}}, UpdateState).

%%-----------------------------------------------------------------
%% Makes a asynchronous call to a generic process.
%% Request is sent to the Pid, and the response must be
%% {MRef, Reply}.
%%-----------------------------------------------------------------

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
call({_Name, Node}=Process, Label, Request) when is_atom(Node) ->
    do_call(Process, Label, Request).

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
