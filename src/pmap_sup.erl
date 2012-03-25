%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.wd201201>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2012 by Chen Slepher <slepher@larry.wd201201>
%%%-------------------------------------------------------------------
-module(pmap_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'pmap_monitor', {'pmap_monitor', start_link, []}, Restart, Shutdown, Type, ['pmap_monitor']},
    PWSup  =  {'pmap_worker_sup', {supervisor, start_link,
                                  [{local, pmap_worker_sup}, ?MODULE, [pmap_worker_sup]]},
               transient, infinity, supervisor, []},
    AWSup  =  {'atask_worker_sup', {supervisor, start_link,
                                    [{local, atask_worker_sup}, ?MODULE, [atask_worker_sup]]},
               transient, infinity, supervisor, []},
    {ok, {SupFlags, [AChild, PWSup, AWSup]}};

init([pmap_worker_sup]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{undefined, {pmap_worker, start_link, []}, temporary, 5000, worker, [pmap_worker]}]
         }};
init([atask_worker_sup]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{undefined, {atask_worker, start_link, []}, temporary, 5000, worker, [atask_worker]}]
         }}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
