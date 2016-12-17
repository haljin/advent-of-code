-module(day17).

-compile(export_all).

input() -> "gdjjyniy".


solve() ->
  solve([{{1, 1}, ""}], undefined).

solve2() ->
  solve2([{{1, 1}, ""}], undefined).

solve([], Min) ->
  Min;
solve([{_State, Steps} | Rest], Min) when Min =/= undefined,
                                          length(Min) < length(Steps) ->
  solve(Rest, Min);
solve([{State, Steps} | Rest], Min) ->
  case {is_final(State), Min} of
    {true, undefined} ->
      solve(Rest, Steps);
    {true, Min} ->
      solve(Rest, path_min(Min, Steps));
    {false, _} ->
      PossibleMoves = possible_moves(State, Steps),
      NewStates = [apply_state_change(Move, State, Steps) || Move <- PossibleMoves],
      solve(Rest ++ NewStates, Min)
  end.

solve2([], Max) ->
  Max;
solve2([{State, Steps} | Rest], Max) ->
  case {is_final(State), Max} of
    {true, undefined} ->
      solve2(Rest, length(Steps));
    {true, Max} ->
      solve2(Rest, max(Max, length(Steps)));
    {false, _} ->
      PossibleMoves = possible_moves(State, Steps),
      NewStates = [apply_state_change(Move, State, Steps) || Move <- PossibleMoves],
      solve2(Rest ++ NewStates, Max)
  end.

is_final({4, 4}) ->
  true;
is_final(_Else) ->
  false.

apply_state_change($U, {X, Y}, Path) ->
  {{X, Y - 1}, Path ++ [$U]};
apply_state_change($D, {X, Y}, Path) ->
  {{X, Y + 1}, Path ++ [$D]};
apply_state_change($R, {X, Y}, Path) ->
  {{X + 1, Y}, Path ++ [$R]};
apply_state_change($L, {X, Y}, Path) ->
  {{X - 1, Y}, Path ++ [$L]}.

possible_moves({X, Y}, Path) ->
  <<UpAllowed:4, DownAllowed:4, LeftAllowed:4, RightAllowed:4, _Rest/bits>> = erlang:md5(input() ++ Path),
  Moves = [{{X, Y - 1}, UpAllowed, $U}, {{X, Y + 1}, DownAllowed, $D}, {{X - 1, Y}, LeftAllowed, $L}, {{X + 1, Y}, RightAllowed, $R}],
  [M || {{NewX, NewY}, DirectionAllowed, M} <- Moves, (NewX >= 1 andalso NewX =< 4) andalso
                                                      (NewY >= 1 andalso NewY =< 4) andalso
    allowed(DirectionAllowed)].


allowed(Val) when Val > 10 ->
  true;
allowed(_Else) ->
  false.

path_min(Path1, Path2) when length(Path1) < length(Path2) -> Path1;
path_min(_, Path2) -> Path2.