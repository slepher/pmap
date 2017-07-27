%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(not_work_well).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    pmap:start(),
    {ok, Server} = echo_server:start(),
    _R = pmap:async_task(
           fun(N) -> echo_server:delayed_echo(
                       Server, 1000, N) end, lists:seq(1, 200), 10),
    [{_, Pid, _, _}] = supervisor:which_children(pmap_worker_sup),
    io:format("pmap_worker pid is ~p, self is ~p ~n", [Pid, self()]),
    timer:sleep(5000),
    io:format("sleep end, expect to finish "),
    Pid ! {callbacks_info, self()},
    wait_receive(),
    init:stop().


wait_receive() ->
    receive 
        M ->
            wait_receive()
    after 100 ->
            ok
    end.
              

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
