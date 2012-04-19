%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@larry.local>
%%% @copyright (C) 2012, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Feb 2012 by Chen Slepher <slepher@larry.local>
%%%-------------------------------------------------------------------
-module(pmap_worker).

-behaviour(gen_server).

%% API
-export([task/5, async_task/5, monitor_task/6, progress/1]).
-export([start/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {callbacks, completed = 0, acc, working, pending}).

%%%===================================================================
%%% API
%%%===================================================================
task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    atask:start_and_action(fun start/0, fun gen_server:call/3,
                           [{task, TaskHandler, ReplyHandler, Acc0, Items, Limit, undefined}, infinity]).

async_task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    atask:start_and_action(fun start/0, fun atask_gen_server:call/2,
                           [{task, TaskHandler, ReplyHandler, Acc0, Items, Limit, undefined}]).

monitor_task(From, TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    atask:start_and_action(fun start/0, fun atask_gen_server:call/2,
                           [{task, TaskHandler, ReplyHandler, Acc0, Items, Limit, From}]).

progress(PId) ->
    pmap_util:async_gen_server_call(PId, progress).

start() ->
    supervisor:start_child(pmap_worker_sup, []).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    {ok, #state{callbacks = dict:new()}}.

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

handle_call({task, TaskHandler, ReplyHandler, Acc0, Items, Limit, Monitor}, From, State) ->
    {WorkingItems, PendingItems} = split_items(Items, Limit),
    message_monitor(Monitor, From, {0, WorkingItems}),
    NState = 
        lists:foldl(
          fun(Item, S) ->
                  add_task(Item, TaskHandler, ReplyHandler, Monitor, From, S)
          end, State#state{acc = Acc0, working = WorkingItems, pending = PendingItems}, WorkingItems),
    {noreply, NState};

handle_call(progress, _From, #state{completed = Completed, working = Working} = State) ->
    {reply, {ok, {Completed, Working}}, State};
                                   
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
handle_info({message, MRef, Response}, State) when is_reference(MRef) ->
    atask_gen_server:handle_reply({message, MRef, Response}, #state.callbacks, State);
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

next_item([Head|T]) ->
    {Head, T};
next_item([]) ->
    done;
next_item(Continuation) when is_function(Continuation) ->
    Continuation().
        
split_items(Items, Limit) when length(Items) >= Limit, Limit > 0 ->
    lists:split(Limit, Items);
split_items(Items, _Limit) when is_list(Items) ->
    {Items, []};
split_items(Continuation, Limit) when is_function(Continuation, Limit) ->
    split_items(Continuation, Limit, []).

split_items(Continuation, 0, Acc) ->
    {Acc, Continuation};
split_items(Continuation, Limit, Acc) ->
    case Continuation() of
        {continucation, Item, NContinucation} ->
            split_items(NContinucation, Limit - 1, [Item|Acc]);
        done ->
            {Acc, []}
    end.

message_monitor(undefined, _, _) ->
    ok;
message_monitor(_Monitor, From, Reply) ->
    atask_gen_server:message(From, Reply).

reply(undefined, From, Reply) ->
    gen_server:reply(From, Reply);
reply(Monitor, From, Reply) ->
    gen_server:reply(Monitor, Reply),
    gen_server:reply(From, done).


add_task(Item, TaskHandler, ReplyHandler, Monitor, From, State) ->
    Callback =
        fun(Reply, #state{acc = Acc, working = WIs, pending = PIs, completed = C} = S) ->
                NAcc = ReplyHandler(Item, Reply, Acc),
                NWIs = lists:delete(Item, WIs),
                NS = S#state{acc = NAcc, completed = C + 1},
                case next_item(PIs) of
                     done ->
                         NNS = NS#state{working = NWIs, pending = []},
                         case NWIs of
                             [] ->
                                 reply(Monitor, From, NAcc),
                                 {stop, normal, NNS};
                             _ ->
                                 message_monitor(Monitor, From, {C + 1, NWIs}),
                                 NNS
                         end; 
                     {TaskItem, NPIs} ->
                        NNWIs = [TaskItem|NWIs],
                        message_monitor(Monitor, From, {C + 1,NNWIs}),
                        add_task(TaskItem, TaskHandler, ReplyHandler, Monitor, From,
                                 NS#state{working = NNWIs, pending = NPIs})
                 end
        end,
    atask_gen_server:wait_reply(Callback, TaskHandler(Item), #state.callbacks, State).
