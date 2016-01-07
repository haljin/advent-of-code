-module(day1).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve(Input) ->
  process_day1(Input, 0).

process_day1([$( | T], Acc) ->
  process_day1(T, Acc +1);
process_day1([$) | T], Acc) ->
  process_day1(T, Acc -1);
process_day1([], Acc) ->
  Acc.

solve2(Input) ->
  process_day1_2(Input, 0, 1).

process_day1_2(_, Acc, Pos) when Acc < 0 ->
  Pos - 1;
process_day1_2([$( | T], Acc, Pos) ->
  process_day1_2(T, Acc +1, Pos + 1);
process_day1_2([$) | T], Acc, Pos) ->
  process_day1_2(T, Acc -1, Pos + 1);
process_day1_2([], _Acc, Pos) ->
  Pos.
