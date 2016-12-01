-module(day17).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File,
                       fun(Line, Acc) ->
                          [list_to_integer(Line -- "\n") | Acc]
                        end, [read], []).


solve(Containers, Sum) ->
   length(fill(Containers, Sum, [])).

solve2(Containers, Sum) ->
  Solutions = fill(Containers, Sum, []),
  MinLength = lists:min([length(Sol) || Sol <- Solutions]),
  length([S || S <- Solutions, length(S) =:= MinLength]).


fill([], _Sum, Results) ->
  Results;
fill([Sum | T], Sum, Results) ->
  fill(T, Sum, [[Sum] | Results]);
fill([H|T], Sum, Results) when H < Sum ->
  SubResults = fill(T, Sum - H, []),
  FillResults = [[H| R] || R <- SubResults],
  fill(T, Sum, FillResults ++ Results);
fill([H|T], Sum, Results) when H > Sum ->
  fill(T, Sum, Results).

