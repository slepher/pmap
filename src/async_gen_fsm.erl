%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_gen_fsm).

%% API
-export([sync_send_event/2, sync_send_all_state_event/2]).
-export([promise_sync_send_event/2, promise_sync_send_event/3]).
-export([promise_sync_send_all_state_event/2, promise_sync_send_all_state_event/3]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
sync_send_event(Process, Request) ->
    async:call(Process, '$gen_sync_event', Request).

sync_send_all_state_event(Process, Request) ->
    async:call(Process, '$gen_sync_all_state_event', Request).

promise_sync_send_event(Process, Request) ->
    promise_sync_send_event(Process, Request, infinity).

promise_sync_send_event(Process, Request, Timeout) ->
    async:promise_action(
      fun() ->
              sync_send_event(Process, Request)
      end, Timeout).

promise_sync_send_all_state_event(Process, Request) ->
    promise_sync_send_all_state_event(Process, Request, infinity).

promise_sync_send_all_state_event(Process, Request, Timeout) ->
    async:promise_action(
      fun() ->
              sync_send_all_state_event(Process, Request)
      end, Timeout).
%%%===================================================================
%%% Internal functions
%%%===================================================================
