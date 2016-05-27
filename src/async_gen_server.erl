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
    atask:call(Process, '$gen_call', Request).

promise_call(Process, Request) ->
    promise_call(Process, Request, infinity).

promise_call(Process, Request, Timeout) ->
    async_m:promise(call(Process, Request), Timeout).

message({PId, MRef}, Message) ->
    async_m:message({PId, MRef}, Message).
%%%===================================================================
%%% Internal functions
%%%===================================================================
