-module(day18).

-compile(export_all).

input() -> ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^....".

test_input() -> ".^^.^.^^^^".

solve() ->
  StartSafeTiles = length([$. || $. <- input()]),
  solve(2, [$. | input()] ++ [$.], [], StartSafeTiles, 40).

solve2() ->
  StartSafeTiles = length([$. || $. <- input()]),
  solve(2, [$. | input()] ++ [$.], [], StartSafeTiles, 400000).

solve(N, [_, _], NextRow, SafeTiles, MaxRows) when N =< MaxRows ->
  solve(N + 1, [$. | NextRow] ++ [$.], [], SafeTiles + length([$. || $. <- NextRow]), MaxRows);
solve(_, [_, _], _, SafeTiles, _MaxRows) ->
  SafeTiles;
solve(N, [$^, $^, $. | Rest], NextRow, SafeTiles, MaxRows) ->
  solve(N, [$^, $. | Rest], [$^ | NextRow], SafeTiles, MaxRows);
solve(N, [$., $^, $^ | Rest], NextRow, SafeTiles, MaxRows) ->
  solve(N, [$^, $^ | Rest], [$^ | NextRow], SafeTiles, MaxRows);
solve(N, [$^, $., $. | Rest], NextRow, SafeTiles, MaxRows) ->
  solve(N, [$., $. | Rest], [$^ | NextRow], SafeTiles, MaxRows);
solve(N, [$., $., $^ | Rest], NextRow, SafeTiles, MaxRows) ->
  solve(N, [$., $^ | Rest], [$^ | NextRow], SafeTiles, MaxRows);
solve(N, [_ | Rest], NextRow, SafeTiles, MaxRows) ->
  solve(N, Rest, [$. | NextRow], SafeTiles, MaxRows).





