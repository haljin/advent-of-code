-module(day10).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve(InputString, Repetitions) when Repetitions > 0 ->
  Output = process_day10(InputString, undefined, 0, []),
  solve(Output, Repetitions -1 );
solve(InputString, 0) ->
  length(InputString).

process_day10([Char | T], Char, Count, OutputString) ->
  process_day10(T, Char, Count + 1, OutputString);
process_day10([OtherChar | T], undefined, _Count, OutputString) ->
  process_day10(T, OtherChar, 1, OutputString);
process_day10([OtherChar | T], Char, Count, OutputString) ->
  process_day10(T, OtherChar, 1, [Char] ++ integer_to_list(Count) ++ OutputString);
process_day10([], Char, Count, OutputString) ->
  lists:reverse([Char] ++ integer_to_list(Count) ++ OutputString).