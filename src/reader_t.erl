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

-module(reader_t).
-compile({parse_transform, do}).
-behaviour(monad_trans).

-export_type([reader_t/3]).

-export([new/1, '>>='/3, return/2, fail/2, lift/2, ask/1]).

-opaque reader_t(R, M, A) :: fun( (A) -> monad:monadic(M, R)).


-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(M) ->
    {?MODULE, M}.


-spec '>>='(reader_t(R, M, A), fun( (A) -> reader_t(R, M, B) ), M) -> reader_t(R, M, B).
'>>='(X, Fun, {?MODULE, M}) ->
    fun(R) ->
            do([M || 
                   A <- X(R),
                   (Fun(A))(R)
               ])
    end.


-spec return(A, M) -> reader_t(_R, M, A).
return(A, {?MODULE, M}) ->
    fun (_) ->
            M:return(A)
    end.


-spec fail(any(), M) -> reader_t(_R, M, _A).
fail(E, {?MODULE, M}) ->
    fun (_) ->
            M:fail(E)
    end.

-spec lift(monad:monadic(M, A), M) -> reader_t(_R, M, A).
lift(X, {?MODULE, _M}) ->
    fun(_) ->
            X
    end.

-spec ask(M) -> reader_t(R, M, R).
ask({?MODULE, M}) ->
    fun(R) ->
            M:return(R)
    end.
