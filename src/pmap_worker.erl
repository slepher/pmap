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
-export([task/5, async_task/5, progress/1]).
-export([apply_task_handler/3, apply_reply_handler/5]).
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
task(_TaskHandler, _ReplyHandler, Acc0, [], _Limit) ->
    Acc0;
task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    atask:start_and_action(fun start/0, fun gen_server:call/3,
                           [{task, TaskHandler, ReplyHandler, Acc0, Items, Limit}, infinity]).

async_task(_TaskHandler, _ReplyHandler, Acc0, [], _Limit) ->
    Acc0;
async_task(TaskHandler, ReplyHandler, Acc0, Items, Limit) ->
    atask:start_and_action(fun start/0, fun atask_gen_server:call/2,
                           [{task, TaskHandler, ReplyHandler, Acc0, Items, Limit}]).

progress(PId) ->
    pmap_util:async_gen_server_call(PId, progress).

start() ->
    supervisor:start_child(pmap_worker_sup, []).

apply_task_handler(TaskHandler, Item, From) ->
    case erlang:fun_info(TaskHandler, arity) of
        {arity, 1} ->
            TaskHandler(Item);
        {arity, 2} ->
            TaskHandler(Item, From);
        {arity, N} ->
            exit({invalid_task_handler, TaskHandler, N})
    end.

apply_reply_handler(ReplyHandler, Item, Reply, From, Acc) ->
    case erlang:fun_info(ReplyHandler, arity) of
        {arity, 3} ->
            ReplyHandler(Item, Reply, Acc);
        {arity, 4} ->
            ReplyHandler(Item, Reply, From, Acc);
        {arity, N} ->
            exit({invalid_reply_handler, ReplyHandler, N})
    end.


    
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

handle_call({task, _TaskHandler, _ReplyHandler, Acc0, [], _Limit}, From, State) ->
    gen_server:reply(From, Acc0),
    {stop, normal, State};

handle_call({task, TaskHandler, ReplyHandler, Acc0, Items, Limit}, From, State) ->
    {WorkingItems, PendingItems} = split_items(Items, Limit),
    NState = 
        lists:foldl(
          fun(Item, S) ->
                  add_task(Item, TaskHandler, ReplyHandler, From, S)
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

add_task(Item, TaskHandler, ReplyHandler, From, State) ->
    Callback =
        fun(Reply, #state{acc = Acc, working = WIs, pending = PIs, completed = C} = S) ->
                NAcc = apply_reply_handler(ReplyHandler, Item, Reply, From, Acc),
                case Reply of
                    {message, _Message} ->
                        S#state{acc = NAcc};
                    _ ->
                        NWIs = lists:delete(Item, WIs),
                        NS = S#state{acc = NAcc, completed = C + 1},
                        case next_item(PIs) of
                            done ->
                                NNS = NS#state{working = NWIs, pending = []},
                                case NWIs of
                                    [] ->
                                        gen_server:reply(From, NAcc),
                                        {stop, normal, NNS};
                                    _ ->
                                        NNS
                                end; 
                            {TaskItem, NPIs} ->
                                NNWIs = [TaskItem|NWIs],
                                add_task(TaskItem, TaskHandler, ReplyHandler, From,
                                         NS#state{working = NNWIs, pending = NPIs})
                        end
                end
        end,
    case apply_task_handler(TaskHandler, Item, From) of
        % for old functions
        {ok, MRef} when is_reference(MRef) ->
            atask_gen_server:wait_reply(Callback, MRef, #state.callbacks, State);
        MRef when is_reference(MRef) ->
            atask_gen_server:wait_reply(Callback, MRef, #state.callbacks, State);
        Other ->
            Callback(Other, State)
    end.
