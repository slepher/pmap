%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_m_v5).
-behaviour(monad).
-compile({parse_transform, do}).
%% API
-export(['>>='/2, return/1, fail/1]).
-export([promise/2, run/4, modify/1, execute_cc/4, callback_to_cc/1, handle_info/3]).
-export([promise_call/2, promise_call/3]).

%%%===================================================================
%%% API
%%%===================================================================
'>>='(X, Fun) ->
    M = error_t:new(cont_t:new(async_r_m_v5)),
    M:'>>='(X, Fun).

return(A) ->
    M = error_t:new(cont_t:new(async_r_m_v5)),
    M:return(A).

fail(R) ->
    M = error_t:new(cont_t:new(async_r_m_v5)),
    M:fail(R).

promise(Mref, Timeout) when is_reference(Mref) ->
    promise(fun() -> Mref end, Timeout);
promise(Action, Timeout) when is_function(Action) ->
    fun(CC) ->
            Mref = Action(),
            do([async_r_m_v5 ||
                   State <- async_r_m_v5:get(),
                   Offset <- async_r_m_v5:ask(),
                   begin
                       NCC = cc_with_timeout(CC, Mref, Timeout),
                       CCs = element(Offset, State),
                       NCCs = maps:put(Mref, NCC, CCs),
                       async_r_m_v5:put(setelement(Offset, State, NCCs))
                   end
               ])
    end.

promise_call(Process, Request) ->
    promise_call(Process, Request, infinity).

promise_call(Process, Request, Timeout) ->
  promise(fun() -> async_gen_server_call(Process, Request) end, Timeout).    

run(Promise, CC, Offset, State) ->
    async_r_m_v5:run((Promise)(CC), Offset, State).

async_gen_server_call(Process, Request) ->
    do_call(Process, '$gen_call', Request).

do_call(Process, Label, Request) ->
    Mref = erlang:monitor(process, Process),
    erlang:send(Process, {Label, {self(), Mref}, Request}, [noconnect]),
    Mref.

modify(S) ->
    MC = cont_t:new(async_r_m_v5),
    M = error_t:new(MC),
    M:lift(MC:lift(async_r_m_v5:modify(S))).

execute_cc(CC, Reply, Offset, State) ->
    async_r_m_v5:run(CC(Reply), Offset, State).

cc_with_timeout(CC, _Mref, infinity) ->
    CC;
cc_with_timeout(CC, Mref, Timeout) when is_integer(Timeout), (Timeout > 0) ->
    Timer = erlang:send_after(Timeout, self(), {Mref, {error, timeout}}),
    fun(A) ->
            erlang:cancel_timer(Timer),
            CC(A)
    end.

callback_to_cc(Callback) when is_function(Callback, 0) ->
    fun(_A) ->
            Callback(),
            async_r_m_v5:return(ok)
    end;
callback_to_cc(Callback) when is_function(Callback, 1) ->
    fun(A) ->
            Callback(A),
            async_r_m_v5:return(ok)
    end;
callback_to_cc(Callback) when is_function(Callback, 2) ->
    fun(A) ->
            async_r_m_v5:modify(
              fun(State) ->
                      Callback(A, State)
              end)
    end;
callback_to_cc(Callback) ->
    exit({invalid_callback, Callback}).

handle_info({Mref, Reply}, Offset, State) when is_reference(Mref) ->
    erlang:demonitor(Mref, [flush]),
    CCs = element(Offset, State),
    case maps:find(Mref, CCs) of
        {ok, CC} ->
            NCCs = maps:remove(Mref, CCs),
            NState = setelement(Offset, State, NCCs),
            execute_cc(CC, Reply, Offset, NState);
        error ->
            State
    end;
handle_info({'DOWN', Mref, _, _, Reason}, Offset, State) when is_reference(Mref) ->
    handle_info({Mref, {error, {process_down, Reason}}}, Offset, State);
handle_info(_Info, _Offset, _State) ->
    unhandled.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
