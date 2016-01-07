-module(day4).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve(Input) ->
  process_day4(Input, 1).

process_day4(Input, N) ->
  case erlang:md5(Input ++ integer_to_list(N)) of
    <<0, 0, ThirdByte, _Rest/binary>> when ThirdByte < 15 ->
      N;
    _ ->
      process_day4(Input, N + 1)
  end.

solve2(Input) ->
  process_day4_2(Input, 1).

process_day4_2(Input, N) ->
  case erlang:md5(Input ++ integer_to_list(N)) of
    <<0, 0, 0, _Rest/binary>> ->
      N;
    _ ->
      process_day4_2(Input, N + 1)
  end.


print_hash(Bin) ->
  [io:format("~2.16.0B", [Char]) || Char <- binary_to_list(Bin)].