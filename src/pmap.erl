%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
-module(pmap).

%% API
-export([start/0]).
-export([atask/1]).
-export([map/2, map/3]).
-export([task/2, task/3, task/5]).
-export([async_task/2, async_task/3, async_task/5]).
-export([monitor_task/2, monitor_task/3, monitor_task/5, status/1]).
-export([simple_callback/3, succfail_callback/3]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(pmap).

atask(F) ->
    atask_worker:atask(F).

map(F, Items) ->
    map(F, Items, 0).

map(F, Items, Limit) ->
    Length = length(Items),
    Handler = fun(N) -> Item = lists:nth(N, Items), pmap:atask(fun() -> F(Item) end) end,
    [V|| {_N, V} <- lists:usort(task(Handler, lists:seq(1, Length), Limit))].

task(TaskHandler, Items) ->
    task(TaskHandler, Items, 0).

task(TaskHandler, Items, Limit) ->
    task(TaskHandler, fun simple_callback/3, [], Items, Limit).

task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    pmap_worker:task(TaskHandler, ReplyHandler, Acc0, Items, Limit).

async_task(TaskHandler, Items) ->
    async_task(TaskHandler, Items, 0).

async_task(TaskHandler, Items, Limit) ->
    async_task(TaskHandler, fun simple_callback/3, [], Items, Limit).

async_task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    pmap_worker:async_task(TaskHandler, ReplyHandler, Acc0, Items, Limit).

monitor_task(TaskHandler, Items) ->
    monitor_task(TaskHandler, Items, 0).

monitor_task(TaskHandler, Items, Limit) ->
    monitor_task(TaskHandler, fun simple_callback/3, [], Items, Limit).

monitor_task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    pmap_monitor:async_task(TaskHandler, ReplyHandler, Acc0, Items, Limit).

status(MRef) when is_reference(MRef) ->
    pmap_monitor:status(MRef).

simple_callback(Item, Reply, Acc) ->
    [{Item, Reply}|Acc].

succfail_callback(Item, Reply, {Succs, Fails}) ->
    case Reply of
        {ok, Val} ->
            {[{Item, Val}|Succs], Fails};
        {error, Reason} ->
            {Succs, [{Item, Reason}|Fails]}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
