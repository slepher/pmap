%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Dec 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(echo_server).

-behaviour(gen_server).

%% API
-export([delayed_echo/3]).
-export([map_map/1]).
-export([start/0, start/1, start_link/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {callbacks = maps:new()}).

%%%===================================================================
%%% API
%%%===================================================================
map_map(N) ->
    case echo_server:start() of
        {ok, Pid} ->
            async_gen_server:call(Pid, {map, {map, {delayed_echo, hello, 2000}, N}, N});
        {error, Reason} ->
            {error, Reason}
    end.

delayed_echo(PName, Request, Timeout) ->
    gen_server:call(PName, {delayed_echo, Request, Timeout}, infinity).

start() ->
    start(undefined).

start(PName) ->
    supervisor:start_child(echo_server_sup, [PName]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(undefined) ->
    gen_server:start_link(?MODULE, [], []);

start_link(PName) ->
    gen_server:start_link({local, PName}, ?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_call({map, Request, N}, From, State) ->
    Self = self(),
    Monad = 
        pmap:promise_task(
          fun(_N) ->
                  case echo_server:start() of
                      {ok, Pid} ->
                          pmap_channel_worker:request(channel, Pid, Request, Self);
                      {error, Reason} ->
                          {error, Reason}
                  end
          end, lists:seq(1, N), 20),
    NState = async_m:then(Monad, 
                          fun(Reply) ->
                                  gen_server:reply(From, Reply)
                          end, #state.callbacks, State),
    {noreply, NState};

handle_call({delayed_echo, Request, Timeout}, From, State) ->
    erlang:send_after(Timeout, self(), {reply, From, Request}),
    {noreply, State};
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
handle_info({reply, From, Request}, State) ->
    gen_server:reply(From, Request),
    {noreply, State};
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
