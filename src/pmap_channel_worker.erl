%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Dec 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(pmap_channel_worker).

-behaviour(gen_server).

%% API
-export([start/2, start_link/2]).
-export([request/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {callbacks  = maps:new(), pending_actions = [], workings = sets:new(), working_parents = sets:new(), pool_size}).

%%%===================================================================
%%% API
%%%===================================================================

request(Channel, Pid, Request, FromPid) ->
    PName = pname(Channel),
    async_gen_server:call(PName, {action, Pid, Request, FromPid}).

pname(Channel) ->
    list_to_atom(lists:flatten(["pmap_worker_channel_", atom_to_list(Channel)])).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Channel, PoolSize) ->
    supervisor:start_child(pmap_channel_worker_sup, [Channel, PoolSize]).

start_link(Channel, PoolSize) ->
    PName = pname(Channel),
    gen_server:start_link({local, PName}, ?MODULE, [PoolSize], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([PoolSize]) ->
    {ok, #state{pool_size = PoolSize}}.

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
handle_call({action, Pid, Request, FromPid}, From, #state{pending_actions = T} = State) ->
    NState = process_next_action(State#state{pending_actions = [{Pid, Request, FromPid, From}|T]}),
    {noreply, NState};
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
handle_info(Info, State) ->
    case async_m:handle_info(Info, #state.callbacks, State) of
        unhandled ->
            error_logger:error_msg("unexpected info msg ~p", [Info]),
            {noreply, State};
        NState when is_record(NState, state) ->
            {noreply, NState}
    end.

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

process_next_action(#state{pending_actions = []} = State) ->
    State;
process_next_action(#state{pending_actions = [{Pid, Request, ParentPid, From}|T], 
                           workings = Working, working_parents = WorkingParents, pool_size = PoolSize } = State) ->
    NWorkingParents = 
        case sets:is_element(ParentPid, Working) of
            true ->
                sets:add_element(ParentPid, WorkingParents);
            false ->
                WorkingParents
        end,
    NWorking = sets:add_element(Pid, Working),
    WorkingLen = sets:size(NWorking) - sets:size(NWorkingParents),
    case WorkingLen > PoolSize of
        true ->
            State;
        false ->
            process_action(Pid, Request, From, State#state{pending_actions = T, workings = NWorking, working_parents = NWorkingParents})
    end.

process_action(Pid, Request, From, State) ->
    Monad = async_gen_server:promise_call(Pid, Request),
    Callback = fun({message, M}, S) ->
                       async:message(From, M),
                       S;
                  (Reply, #state{workings = Working, working_parents = WorkingParents} = S) ->
                       gen_server:reply(From, Reply),
                       NWorking = sets:del_element(Pid, Working),
                       NWorkingParents = sets:del_element(Pid, WorkingParents),
                       process_next_action(S#state{workings = NWorking, working_parents = NWorkingParents})
               end,
    async_m:then(Monad, Callback, #state.callbacks, State).
