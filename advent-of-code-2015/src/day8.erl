-module(day8).

-compile(export_all).

parse_data(Path) ->
  santa_utils:map_file(Path, fun(X, Acc) ->
    Entry = string:strip(X, both, $\n),
    Acc ++ [Entry]
                              end, [read], []).

solve(ListofStrings) ->
  process_day8(ListofStrings, 0, 0, 0).

process_day8([String | T], Real, Memory, Encoded) ->
  {ok, Tokens, _} = erl_scan:string(String ++ "."),
  {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
  {value, MemString, _} = erl_eval:exprs(AbsForm, []),
  [EncString] = io_lib:format("~p", [String]),
  process_day8(T, Real + length(String), Memory + length(MemString), Encoded + length(EncString));
process_day8([], Real, Memory, Encoded) ->
  {Real - Memory, Encoded - Real}.