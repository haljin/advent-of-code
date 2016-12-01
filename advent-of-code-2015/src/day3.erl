-module(day3).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve(Input) ->
  process_day3(Input, [{0, 0}], {0, 0}).

process_day3([Char | T], Acc, Loc) ->
  NewLoc = new_location(Char, Loc),
  process_day3(T, [NewLoc | Acc], NewLoc);
process_day3([], Acc, _) ->
  length(lists:usort(Acc)).

solve2(Input) ->
  process_day3_2(Input, [{0, 0}], {0, 0}, {0, 0}, santa).

process_day3_2([Char | T], Visited, SantaLoc, RoboLoc, santa) ->
  NewLoc = new_location(Char, SantaLoc),
  process_day3_2(T, [NewLoc | Visited], NewLoc, RoboLoc, robosanta);
process_day3_2([Char | T], Visited, SantaLoc, RoboLoc, robosanta) ->
  NewLoc = new_location(Char, RoboLoc),
  process_day3_2(T, [NewLoc | Visited], SantaLoc, NewLoc, santa);
process_day3_2([], Visited, _, _, _) ->
  length(lists:usort(Visited)).

new_location($^, {SantaX, SantaY}) ->
  {SantaX, SantaY + 1};
new_location($v, {SantaX, SantaY}) ->
  {SantaX, SantaY - 1};
new_location($<, {SantaX, SantaY}) ->
  {SantaX - 1, SantaY};
new_location($>, {SantaX, SantaY}) ->
  {SantaX + 1, SantaY}.