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

-module(cont_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).

-export_type([cont_t/3]).

-export([new/1, '>>='/3, return/2, fail/2, lift/2, callCC/2]).

-opaque cont_t(R, M, A) :: fun((fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R) ).

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.


-spec '>>='(cont_t(R, M, A), fun( (A) -> cont_t(R, M, B) ), M) -> cont_t(R, M, B).
'>>='(X, Fun, {?MODULE, _M}) ->
    fun(K) ->
            X(fun(A) ->
                      (Fun(A))(K)
              end)
    end.

-spec return(A, M) -> cont_t(_R, M, A).
return(A, {?MODULE, _M}) ->
    fun (K) ->
            K(A)
    end.

-spec fail(any(), M) -> cont_t(_R, M, _A).
fail(E, {?MODULE, M}) ->
    fun (_) ->
            M:fail(E)
    end.

-spec lift(monad:monadic(M, A), M) -> cont_t(_R, M, A).
lift(X, {?MODULE, M}) ->
    fun(F) ->
            M:'>>='(X, F)
    end.

-spec callCC(fun((fun( (A) -> cont_t(R, M, _B) ))-> cont_t(R, M, A)), M) -> cont_t(R, M, A).
callCC(F, {?MODULE, _M}) ->
    fun(H) ->
            (F(
               fun(A) ->
                       fun(_) ->
                               H(A)
                       end
               end))(H)
    end.
