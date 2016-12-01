-module(day1).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve() ->
  solve(parse_data("../priv/day1input.txt")).

solve2() ->
  solve2(parse_data("../priv/day1input.txt")).

solve(Input) ->
  Tokenised = string:tokens(Input, ", "),
  go(Tokenised, north, {0, 0}).

solve2(Input) ->
  Tokenised = string:tokens(Input, ", "),
  go2(Tokenised, north, {0, 0}, [{0,0}]).

go([], _Facing, Coords) ->
  manhattan({0, 0}, Coords);
go([[Direction |Distance] | Rest], Facing, Coords) ->
  NewFacing = turn(Direction, Facing),
  go(Rest, NewFacing, move(NewFacing, list_to_integer(Distance), Coords)).


go2([], _Facing, Coords, _Visited) ->
  manhattan({0, 0}, Coords);
go2([[Direction | Distance] | Rest], Facing, Coords, Visited) ->
  NewFacing = turn(Direction, Facing),
  case move_inbetween(NewFacing, list_to_integer(Distance), Coords, Visited) of
    {duplicate, DupCoords} ->
      manhattan({0, 0}, DupCoords);
    {no_duplicate, NewCoords, NewVisited} ->
      go2(Rest, NewFacing, NewCoords, NewVisited)
  end.

turn($L, north) -> west;
turn($L, west)  -> south;
turn($L, south) -> east;
turn($L, east)  -> north;
turn($R, north) -> east;
turn($R, east)  -> south;
turn($R, south) -> west;
turn($R, west)  -> north.

move(north, X, {CurX, CurY}) -> {CurX, CurY + X};
move(south, X, {CurX, CurY}) -> {CurX, CurY - X};
move(east, X, {CurX, CurY})  -> {CurX - X, CurY};
move(west, X, {CurX, CurY})  -> {CurX + X, CurY}.

move_inbetween(_, 0, NewCoords, Visited) ->
  {no_duplicate, NewCoords, Visited};
move_inbetween(north, N, {CurX, CurY}, Visited) ->
  NewCoords = {CurX, CurY + 1},
  case lists:member(NewCoords, Visited) of
    true -> {duplicate, NewCoords};
    false -> move_inbetween(north, N - 1, NewCoords, [NewCoords | Visited])
  end;
move_inbetween(south, N, {CurX, CurY}, Visited) ->
  NewCoords = {CurX, CurY - 1},
  case lists:member(NewCoords, Visited) of
    true -> {duplicate, NewCoords};
    false -> move_inbetween(south, N - 1, NewCoords, [NewCoords | Visited])
  end;
move_inbetween(east, N, {CurX, CurY}, Visited) ->
  NewCoords = {CurX + 1, CurY},
  case lists:member(NewCoords, Visited) of
    true -> {duplicate, NewCoords};
    false -> move_inbetween(east, N - 1, NewCoords, [NewCoords | Visited])
  end;
move_inbetween(west, N, {CurX, CurY}, Visited) ->
  NewCoords = {CurX - 1, CurY},
  case lists:member(NewCoords, Visited) of
    true -> {duplicate, NewCoords};
    false -> move_inbetween(west, N - 1, NewCoords, [NewCoords | Visited])
  end.

manhattan({X1, Y1}, {X2, Y2}) ->
  abs(X2 - X1) + abs(Y2 - Y1).

