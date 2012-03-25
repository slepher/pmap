%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.local>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Feb 2012 by Chen Slepher <slepher@larry.local>
%%%-------------------------------------------------------------------
-module(pmap_monitor).

-behaviour(gen_server).

%% API
-export([async_task/5]).
-export([status/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {callbacks, progresses}).

%%%===================================================================
%%% API
%%%===================================================================

async_task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    atask_gen_server:call(?SERVER, {task, TaskHandler, ReplyHandler, Acc0, Items, Limit}).

status(MRef) ->
    receive
        {MRef, Reply} when is_reference(MRef) ->
            erlang:demonitor(MRef, [flush]),
            {ok, Reply};
        {'DOWN', MRef, _, _, Reason} when is_reference(MRef) ->
            {error, {no_such_process, Reason}}
    after 0 ->
        gen_server:call(?SERVER, {progress, MRef})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{progresses = dict:new(), callbacks = dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({task, TaskHandler, ReplyHandler, Acc0, Items, Limit}, {_PId, MRef} = From,
            #state{progresses = ProgressMap} = State) ->
    Async = pmap_worker:monitor_task(From, TaskHandler, ReplyHandler, Acc0, Items, Limit),
    NProgressMap = dict:store(MRef, {0, []}, ProgressMap),
    NState = 
        atask_gen_server:wait_reply(
          fun(Reply, #state{progresses = PMap} = S) ->
                  NPMap = 
                      case Reply of
                          done ->
                              dict:erase(MRef, PMap);
                          {message, Message} ->
                              dict:store(MRef, Message, PMap);
                          {error, Reason} ->
                              gen_server:reply(From, {error, Reason}),
                              dict:erase(MRef, PMap)
                      end,
                  S#state{progresses = NPMap}
          end, Async, #state.callbacks, State#state{progresses = NProgressMap}),
    {noreply, NState};

handle_call({progress, MRef}, _From, #state{progresses = ProgressMap} = State) ->
    Reply =
        case dict:find(MRef, ProgressMap) of
            {ok, Progress} ->
                {progress, Progress};
            error ->
                {error, notask}
        end,
    {reply, Reply, State};
                                   
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({message, MRef, Message}, State) when is_reference(MRef) ->
    atask_gen_server:handle_reply({message, MRef, Message}, #state.callbacks, State);
handle_info({MRef, Response}, State) when is_reference(MRef) ->
    atask_gen_server:handle_reply({MRef, Response}, #state.callbacks, State);
handle_info({'DOWN', MRef, Type, Object, Reason}, State) when is_reference(MRef) ->
    atask_gen_server:handle_reply({'DOWN', MRef, Type, Object, Reason}, #state.callbacks, State);

handle_info(Info, State) ->
    error_logger:info_msg("[~p] unexpected info msg ~p", [?MODULE, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

