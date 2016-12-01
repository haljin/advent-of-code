-module(day1).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve(Input) ->
  ok.
