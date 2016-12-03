-module(day3).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File, fun(X, Acc) ->
    NewTriangle = [list_to_integer(Side) || Side <- string:tokens(X -- "\n", " ")],
    Acc ++ [NewTriangle] end, [read], []).

solve() ->
  solve(parse_data("../priv/day3input.txt"), []).

solve2() ->
  solve(transform_triangles(parse_data("../priv/day3input.txt"), []), []).


transform_triangles([[X1, Y1, Z1], [X2, Y2, Z2], [X3, Y3, Z3] | Rest], Triangles) ->
  transform_triangles(Rest, [[X1, X2, X3], [Y1, Y2, Y3], [Z1, Z2, Z3] | Triangles]);
transform_triangles(_, Triangles) ->
  Triangles.


solve([], Triangles) ->
  length(Triangles);
solve([[X, Y, Z] | Rest], Triangles) when (X + Y) > Z,
                                          (Y + Z) > X,
                                          (X + Z) > Y ->
 solve(Rest, [[X, Y, Z] | Triangles]);
solve([_NotATriangle | Rest], Triangles) ->
  solve(Rest, Triangles).

