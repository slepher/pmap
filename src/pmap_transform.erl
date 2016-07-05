%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2015, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2015 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(pmap_transform).

%% API
-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Ast, _Options) ->
    walk_ast(Ast, []). 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
walk_ast([], Acc) ->
    lists:reverse(Acc);
walk_ast([{attribute, Line, record, {state, Fields}}|T], Acc) ->
    NFields = 
        case has_callbacks(Fields) of
            false ->
                Fields ++ [{record_field, Line,
                            {atom,Line,callbacks},
                            {call,Line,{remote,Line, 
                                        {atom,Line,maps},{atom,Line,new}},[]}}];
            true ->
                Fields
        end,
    walk_ast(T, [{attribute, Line, record, {state, NFields}}|Acc]);
walk_ast([{function, Line, handle_info,2, Clauses}|T], Acc) ->
    NClauses = info_clauses(Line) ++ walk_clauses(Clauses, []),
    walk_ast(T, [{function, Line, handle_info, 2, NClauses}|Acc]);
walk_ast([{function, Line, Function, Arity, Clauses}|T], Acc) ->
    walk_ast(T, [{function, Line, Function, Arity, walk_clauses(Clauses, [])}|Acc]);

walk_ast([Ast|T], Acc) ->
    walk_ast(T, [Ast|Acc]).

walk_clauses([], Acc) ->
    lists:reverse(Acc);
walk_clauses([{clause, Line, Arguments, Guards, Body}|T], Acc) ->
    walk_clauses(T, [{clause, Line, Arguments, Guards, walk_body(Body, [])}|Acc]).

walk_body([], Acc) ->
    lists:reverse(Acc);
walk_body([H|T], Acc) ->
    walk_body(T, [transform_statement(H)|Acc]).

transform_statement({call, Line, {remote, Line1, {atom, Line2, atask_gen_server},
                                  {atom, Line3, update_state}}, Arguments}) ->
    NArguments = 
        case Arguments of
            [Bind, State] ->
                [Bind, {record_index,Line,state,{atom,Line,callbacks}}, State];
            Other ->
                Other
        end,
    {call, Line, {remote, Line1, {atom, Line2, atask_gen_server},
                  {atom, Line3, update_state}}, NArguments};
transform_statement({call, Line, {remote, Line1, {atom, Line2, atask_m},
                                  {atom, Line3, exec}}, Arguments}) ->
    NArguments = 
        case Arguments of
            [Monad, Callback, State] ->
                [Monad, Callback, {record_index,Line,state,{atom,Line,callbacks}}, State];
            Other ->
                Other
        end,
    {call, Line, {remote, Line1, {atom, Line2, atask_m},
                  {atom, Line3, exec}}, NArguments};
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
    Stmt.

                
%%%===================================================================
%%% Internal functions
%%%===================================================================
has_callbacks(Fields) ->
    lists:any(
      fun({record_field, _Line, {atom, _Line, callbacks}}) ->
              true;
         ({record_field, _Line, {atom, _Line, callbacks}, _Call})->
              true;
         (_Other) ->
              false
      end, Fields).

info_clauses(Line) ->
    [{clause,Line,
      [{tuple,Line,[{var,Line,'MRef'},{var,Line,'Reply'}]},{var,Line,'State'}],
      [[{call,Line,{atom,Line,is_reference},[{var,Line,'MRef'}]}]],
      [{call,Line,
        {remote,Line,
         {atom,Line,atask_gen_server},
         {atom,Line,handle_reply}},
        [{tuple,Line,[{var,Line,'MRef'},{var,Line,'Reply'}]},
         {record_index,Line,state,{atom,Line,callbacks}},
         {var,Line,'State'}]}]},
     {clause,Line,
      [{tuple,Line,[{atom,Line,'message'}, 
                    {var,Line,'MRef'},{var,Line,'Reply'}]},{var,Line,'State'}],
      [[{call,Line,{atom,Line,is_reference},[{var,Line,'MRef'}]}]],
      [{call,Line,
        {remote,Line,
         {atom,Line,atask_gen_server},
         {atom,Line,handle_reply}},
        [{tuple,Line,[{atom,Line,'message'},{var,Line,'MRef'},{var,Line,'Reply'}]},
         {record_index,Line,state,{atom,Line,callbacks}},
         {var,Line,'State'}]}]},
     {clause,Line,
      [{tuple,Line,
        [{atom,Line,'DOWN'},
         {var,Line,'MRef'},
         {var,Line,'Type'},
         {var,Line,'Object'},
         {var,Line,'Reason'}]},
       {match,Line,{record,Line,state,[]},{var,Line,'State'}}],
      [],
      [{match,Line,
        {var,Line,'Reply'},
        {tuple,Line,
         [{atom,Line,'DOWN'},
          {var,Line,'MRef'},
          {var,Line,'Type'},
          {var,Line,'Object'},
          {var,Line,'Reason'}]}},
       {call,Line,
        {remote,Line,
         {atom,Line,atask_gen_server},
         {atom,Line,handle_reply}},
        [{var,Line,'Reply'},
         {record_index,Line,state,{atom,Line,callbacks}},
         {var,Line,'State'}]}]}].
