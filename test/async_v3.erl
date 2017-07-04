%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(async_v3).

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
handle_call(request1, From, State) ->
    Callback = 
        fun({ok, Reply}, #state{status = Status} = S) ->
                gen_server:reply(From, {ok, {Status, Reply}}),
                S;
           ({error, Reason}, S) ->
                gen_server:reply(From, {error, Reason}),
                S
        end,
    Promise = many_promise_calls(),
    NState = run(Promise, Callback, #state.callbacks, State),
    {noreply, NState}.

many_promise_calls() ->
  then(then(
       promise1(),
       fun(Reply1) -> 
           chain(promise_with_state(fun(S) -> S#state{status = request2} end), promise2(Reply1))
       end),
       fun(Reply2) ->
             promise3(Reply2)
       end).

promise1() ->
  promise_call(echo_server, {echo, {ok, request1}}).

promise2(Value1) ->
  promise_call(echo_server, {echo, {ok, {Value1, then, request2}}}).

promise3(Value2) ->
  promise_call(echo_server, {echo, {ok, {Value2, then, request3}}}).


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
    NCallback = callback_with_timeout(Callback, Mref, Timeout, Offset),
    Callbacks = element(Offset, State),
    NCallbacks = maps:put(Mref, NCallback, Callbacks),
    setelement(Offset, State, NCallbacks);
wait_reply(Callback, Other, _Timeout, Offset, State) ->
    execute_callback(Callback, Other, Offset, State).

wait_reply_without_state(Callback, Mref, Timeout) ->
    fun(Offset, State) ->
            wait_reply(Callback, Mref, Timeout, Offset, State)
    end.

callback_with_timeout(Callback, _Mref, infinity, _Offset) ->
    Callback;
callback_with_timeout(Callback, Mref, Timeout, Offset) when is_integer(Timeout), (Timeout > 0) ->
    Timer = erlang:send_after(Timeout, self(), {Mref, {error, timeout}}),
    fun(R, S) ->
          erlang:cancel_timer(Timer),
          execute_callback(Callback, R, Offset, S)
  end.

do_handle_info({Mref, Reply}, Offset, State) when is_reference(Mref) ->
    erlang:demonitor(Mref, [flush]),
    Callbacks = element(Offset, State),
    case maps:find(Mref, Callbacks) of
        {ok, Callback} ->
            NCallbacks = maps:remove(Mref, Callbacks),
            NState = setelement(Offset, State, NCallbacks),
            execute_callback(Callback, Reply, Offset, NState);
        error ->
            State
    end;
do_handle_info({'DOWN', Mref, _, _, Reason}, Offset, State) when is_reference(Mref) ->
    do_handle_info({Mref, {error, {process_down, Reason}}}, Offset, State);
do_handle_info(_Info, _Offset, _State) ->
    unhandled.

execute_callback(Callback, Reply, Offset, State) ->
    case erlang:fun_info(Callback, arity) of
        {arity, 1} ->
            case Callback(Reply) of
                WithoutState when is_function(WithoutState) ->
                    WithoutState(Offset, State);
                _Other ->
                    State
            end;
        {arity, 2} ->
            Callback(Reply, State);
        {arity, N} ->
            exit({invalid_callback, Callback, N})
    end.

promise(Action, Timeout) ->
    fun(Callback) ->
            Mref = Action(),
            wait_reply_without_state(Callback, Mref, Timeout)
    end.

promise_call(Process, Request) ->
    promise_call(Process, Request, infinity).

promise_call(Process, Request, Timeout) ->
    promise(fun() -> async_gen_server_call(Process, Request) end, Timeout).

then(Promise, Then) ->
    fun(Callback) ->
            Promise(
              fun({error, Reason}) ->
                      Callback({error, Reason});
                 ({ok, Reply}) ->
                      NPromise = Then(Reply),
                      NPromise(fun(NReply) -> Callback(NReply) end);
                 (Reply) ->
                      NPromise = Then(Reply),
                      NPromise(fun(NReply) -> Callback(NReply) end)
              end)
    end.

chain(Promise1, Promise2) ->
  then(Promise1, fun(_) -> Promise2 end).

promise_with_state(Fun) ->
  fun(Callback) ->
        fun(Offset, State) ->
           NState = Fun(State),
           execute_callback(Callback, {ok, ok}, Offset, NState)
        end
  end.

run(Promise, Callback, Offset, State) ->
    NCallback = 
        fun(A) ->
                fun(O, S) ->
                        execute_callback(Callback, A, O, S)
                end
        end,
    (Promise(NCallback))(Offset, State).
