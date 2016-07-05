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
-export([call/2]).
-export([promise_call/2, promise_call/3]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
call(Process, Request) ->
    async:call(Process, '$gen_call', Request).

promise_call(Name, Request) ->
    promise_call(Name, Request, infinity).

promise_call(Name, Request, Timeout) ->
    async:promise_action(
      fun() ->
              call(Name, Request)
      end, Timeout).
%%%===================================================================
%%% Internal functions
%%%===================================================================
