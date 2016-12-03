-module(day2).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File, fun(X, Acc) -> Acc ++ [X -- "\n"] end, [read], []).

solve() ->
  solve(parse_data("../priv/day2input.txt"), [], keypad()).

solve2() ->
  solve(parse_data("../priv/day2input.txt"), [], keypad2()).

solve([], Code, _Keypad) ->
  lists:reverse(Code);
solve([Line | Rest], Code, Keypad) ->
  solve(Rest, [parse_line(Line, 0, 0, Keypad) | Code], Keypad).

parse_line([], CurX, CurY, Keypad) ->
  maps:get({CurX, CurY}, Keypad);
parse_line([$U | Rest], CurX, CurY, Keypad)  ->
  case maps:is_key({CurX, CurY - 1}, Keypad) of
    true -> parse_line(Rest, CurX, CurY - 1, Keypad);
    false -> parse_line(Rest, CurX, CurY, Keypad)
  end;
parse_line([$D | Rest], CurX, CurY, Keypad) ->
  case maps:is_key({CurX, CurY + 1}, Keypad) of
    true -> parse_line(Rest, CurX, CurY + 1, Keypad);
    false -> parse_line(Rest, CurX, CurY, Keypad)
  end;
parse_line([$R | Rest], CurX, CurY, Keypad) ->
  case maps:is_key({CurX + 1, CurY}, Keypad) of
    true -> parse_line(Rest, CurX + 1, CurY, Keypad);
    false -> parse_line(Rest, CurX, CurY, Keypad)
  end;
parse_line([$L | Rest], CurX, CurY, Keypad) ->
  case maps:is_key({CurX - 1, CurY}, Keypad) of
    true -> parse_line(Rest, CurX - 1, CurY, Keypad);
    false -> parse_line(Rest, CurX, CurY, Keypad)
  end.


keypad() ->
  #{{-1,-1} => $1, {0,-1} => $2, {1,-1} => $3,
    {-1,0} => $4, {0,0} => $5, {1,0} => $6,
    {-1,1} => $7, {0,1} => $8, {1,1} => $9}.


keypad2() ->
#{                              {0, -2} => $1,
                  {-1,-1} => $2,{0,-1} => $3,    {1,-1} => $4,
  {-2, 0} => $5,  {-1,0} => $6, {0,0} => $7,     {1,0} => $8,   {2,0} => $9,
                  {-1,1} => $A, {0, 1} => $B,    {1,1} => $C,
                                {0,2} => $D}.