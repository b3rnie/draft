%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Parse transform, rewrite function call
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(draft_transform).

%%%_* Exports ==========================================================
-export([parse_transform/2]).

-include_lib("draft/include/draft.hrl").

%%%_* Code =============================================================
-record(s, {module, function}).

parse_transform(Forms, _Options) ->
  io:format("FORMS: ~p~n", [Forms]),
  walk_ast(Forms, #s{}, []).

walk_ast([], #s{}, Acc) ->
  lists:reverse(Acc);
walk_ast([{attribute, _Line, module, {Module, _Args}}=H|T], S, Acc) ->
  walk_ast(T, S#s{module=Module}, [H|Acc]);
walk_ast([{attribute, _Line, module, Module}=H|T], S, Acc) ->
  walk_ast(T, S#s{module=Module}, [H|Acc]);
walk_ast([{function, Line, Name, Arity, Clauses}=H|T], S, Acc) ->
  walk_ast(
    T, S, [{function, Line, Name, Arity,
            walk_function(Clauses, S#s{function=Name}, [])}|Acc]);
walk_ast([H|T], S, Acc) ->
  walk_ast(T, S, [H|Acc]).

walk_function([], #s{}, Acc) ->
  lists:reverse(Acc);
walk_function([{clause, Line, Args, Guards, Body}|T], S, Acc) ->
  walk_function(
    T, S, [{clause, Line, Args, Guards,
            walk_function_body(Body, S, [])}|Acc]).

walk_function_body([], S, Acc) ->
  lists:reverse(Acc);
walk_function_body([{call, Line,
                     {remote, _Line1, {atom, _Line2, draft},
                      {atom, _Line3, Severity}}, Args}|T], S, Acc)
  when ?is_draft_severity(Severity) ->
  Info =
    lists:foldl(fun({K,V}, Acc) ->
                    {cons, Line, {tuple, Line, [{atom, Line, K}, V]}, Acc}
                end, {nil, Line},
                [ {module,   {atom, Line, S#s.module}}
                , {function, {atom, Line, S#s.function}}
                , {line,     {integer, Line, Line}}
                ]),
    {F, A} = case Args of
               [_F]     -> {_F, {nil, Line}};
               [_F, _A] -> {_F, _A}
             end,

  Call = call(Line, draft, draft_call, [{atom, Line, Severity}, F, A, Info]),
  walk_function_body(T, S, [Call|Acc]);
walk_function_body([H|T], S, Acc)
  when erlang:is_tuple(H) ->
  walk_function_body(
    T, S, [list_to_tuple(walk_function_body(tuple_to_list(H),S,[]))|Acc]);
walk_function_body([H|T], S, Acc)
  when erlang:is_list(H) ->
  walk_function_body(T, S, [walk_function_body(H, S, []) | Acc]);
walk_function_body([H|T], S, Acc) ->
  walk_function_body(T, S, [H|Acc]).

call(Line, M, F, A) ->
  {call, Line, {remote, Line, {atom, Line, M}, {atom, Line, F}}, A}.

%% Severity | F | A | Module | Function | Line | Pid 
%% transform({call,_Line,Fun,Args}, [[Fun], Args]);

%% node_parse({call,Line,{remote,_,{atom,_,draft},{atom,_,Severity}},Args}, Mods)
%%   when ?is_draft_severity(Severity) ->
%%     ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
