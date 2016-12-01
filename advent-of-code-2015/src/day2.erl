-module(day2).

-compile(export_all).

parse_data(Path) ->
  santa_utils:map_file(Path, fun(X, Acc) ->
    [L, W, H] = string:tokens(string:strip(X, both, $\n), "x"),
    Acc ++ [{list_to_integer(L), list_to_integer(W), list_to_integer(H)}]
                              end, [read], []).

solve(ListOfGifts) ->
  process_day2(ListOfGifts, 0).

process_day2([{L, W, H} | T], Acc) ->
  Sum = 2* L * W + 2* W * H + 2* H * L,
  Min1 = lists:min([L,W,H]),
  Min2 = lists:min([L,W,H] -- [Min1]),
  Extra = Min1 * Min2,
  process_day2(T, Acc + Sum + Extra);
process_day2([], Acc) ->
  Acc.

solve2(ListOfGifts) ->
  process_day2_2(ListOfGifts, 0).

process_day2_2([{L, W, H} | T], Acc) ->
  Min1 = lists:min([L,W,H]),
  Min2 = lists:min([L,W,H] -- [Min1]),
  Sum = 2 * Min1 + 2 * Min2,
  Extra = L * W * H,
  process_day2_2(T, Acc + Sum + Extra);
process_day2_2([], Acc) ->
  Acc.