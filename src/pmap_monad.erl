%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2016, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Aug 2016 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(pmap_monad).
-compile({parse_transform, do}).

%% API
-export([sequencef/4, sequencef2/4]).

%%%===================================================================
%%% API
%%%===================================================================

sequencef(Monad, _AccFun, Acc0, []) ->
    Monad:return(Acc0);
sequencef(Monad, AccFun, Acc0, [H|T]) ->
    do([ Monad ||
           Value <- H,
           begin
               Acc1 = AccFun(Value, Acc0),
               sequencef(Monad, AccFun, Acc1, T)
           end
       ]).

sequencef2(Monad, _Fun, Acc0, []) ->
    Monad:return(Acc0);
sequencef2(Monad, Fun, Acc0, [H|T]) ->
    do([ Monad || 
           Acc1 <- Fun(H, Acc0),
           sequencef2(Monad, Fun, Acc1, T)
       ]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
