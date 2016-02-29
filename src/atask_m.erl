%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(atask_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).
-export([exec/2, exec/4, wait_reply/1]).

%% This is really instance (Error e) => Monad (Either e) with 'error'
%% for Left and 'ok' for Right.

-ifdef(use_specs).
-type(state :: tuple())
-type(cut_state :: fun((integer(), state()) -> state()))
-type(callback(A) :: fun(({'ok', A} | {'error', R}) -> reference() | {'error', R})
-type(monad(A) :: fun((reference(), callback(A)) -> cut_state())
-include("erlando/include/monad_specs.hrl").
-endif.

'>>='(M, Fun) ->
    fun(Callback) ->
            M(
              fun({error, Reason}) ->
                      Callback({error, Reason});
                 (Reply) ->
                      Val = 
                          case Reply of
                              {ok, R} ->
                                  R;
                              ok ->
                                  ok;
                              Other -> 
                                  Other
                          end,
                      (Fun(Val))(
                        fun(NReply) ->
                                Callback(NReply)
                        end)
              end)
    end.    

return(X) -> fun(Callback) -> Callback(X) end.
                       
fail(X) -> throw({error, X}).

wait_reply(Async) ->
    fun(Callback) ->
            atask_gen_server:wait_reply(Async, Callback)
    end.

exec(M, Callback) ->
    M(Callback).

exec(M, Callback, Offset, State) ->
    Bind = exec(M, Callback),
    atask_gen_server:update_state(Bind, Offset, State).
