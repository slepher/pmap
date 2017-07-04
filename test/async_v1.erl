%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_v1).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {callbacks = maps:new(), status}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
handle_call(request0, From, State) ->
    Mref = async_gen_server_call(echo_server, {echo, {ok, request1}}),
    Callback = 
        fun({ok, Reply}, S) ->
                gen_server:reply(From, {ok, Reply}),
                S;
           ({error, Reason}, S) ->
                gen_server:reply(From, {error, Reason}),
                S
        end,
    NState = wait_reply(Callback, Mref, infinity, #state.callbacks, State),
    {noreply, NState};

handle_call(request1, From, State) ->
    Callback = 
        fun({ok, Reply}, S) ->
                gen_server:reply(From, {ok, Reply}),
                S;
           ({error, Reason}, S) ->
                gen_server:reply(From, {error, Reason}),
                S
        end,
    NState = many_async_calls(Callback, State),
    {noreply, NState}.

many_async_calls(Callback, State) ->
    Mref1 = async_gen_server_call(echo_server, {echo, {ok, request1}}),
    wait_reply(
      fun({ok, Reply1}, S) ->
              Mref2 = async_gen_server_call(
                        echo_server, {echo, {ok, {Reply1, then, request2}}}),
              wait_reply(
                fun({ok, Reply2}, S2) ->
                        Mref3 = async_gen_server_call(
                                  echo_server, {echo, {ok, {Reply2, then, request3}}}),
                        wait_reply(Callback, Mref3, infinity, #state.callbacks, S2);
                   ({error, Reason2}, S2) ->
                        io:format("request2 failed ~p~n", [Reason2]),
                        S2
                end, Mref2, infinity, #state.callbacks, S#state{status = request2});
         ({error, Reason1}, S) ->
              io:format("request1 failed ~p~n", [Reason1]),
              S#state{status = request1_failed}
      end, Mref1, infinity, #state.callbacks, State).

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
    case do_handle_info(Info, #state.callbacks, State) of
        unhandled ->
            io:format("unexpected info msg ~p~n", [Info]),
            {noreply, State};
        NState ->
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

async_gen_server_call(Process, Request) ->
    do_call(Process, '$gen_call', Request).

do_call(Process, Label, Request) ->
    Mref = erlang:monitor(process, Process),
    erlang:send(Process, {Label, {self(), Mref}, Request}, [noconnect]),
    Mref.

wait_reply(Callback, Mref, Timeout, Offset, State) when is_reference(Mref) ->
    NCallback = callback_with_timeout(Callback, Mref, Timeout),
    Callbacks = element(Offset, State),
    NCallbacks = maps:put(Mref, NCallback, Callbacks),
    setelement(Offset, State, NCallbacks);
wait_reply(Callback, Other, _Timeout, _Offset, State) ->
    execute_callback(Callback, Other, State).

callback_with_timeout(Callback, _Mref, infinity) ->
    Callback;
callback_with_timeout(Callback, Mref, Timeout) 
  when is_integer(Timeout), (Timeout > 0) ->
    Timer = erlang:send_after(Timeout, self(), {Mref, {error, timeout}}),
    fun(R, S) ->
            erlang:cancel_timer(Timer),
            execute_callback(Callback, R, S)
    end.

do_handle_info({Mref, Reply}, Offset, State) when is_reference(Mref) ->
    erlang:demonitor(Mref, [flush]),
    Callbacks = element(Offset, State),
    case maps:find(Mref, Callbacks) of
        {ok, Callback} ->
            NCallbacks = maps:remove(Mref, Callbacks),
            NState = setelement(Offset, State, NCallbacks),
            execute_callback(Callback, Reply, NState);
        error ->
            State
    end;
do_handle_info({'DOWN', Mref, _, _, Reason}, Offset, State) when is_reference(Mref) ->
    do_handle_info({Mref, {error, {process_down, Reason}}}, Offset, State);
do_handle_info(_Info, _Offset, _State) ->
    unhandled.

execute_callback(Callback, Reply, State) ->
    Callback(Reply, State).
