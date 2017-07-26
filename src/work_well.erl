%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(work_well).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    Start = os:timestamp(),
    pmap:start(),
    {ok, Server} = echo_server:start(),
    pmap:task(fun(N) -> echo_server:delayed_echo(Server, 1000, N) end, lists:seq(1, 200), 10),
    End = os:timestamp(),
    Diff = timer:now_diff(End, Start) div 1000000,
    io:format("diff is ~p~n", [Diff]),
    init:stop().


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
