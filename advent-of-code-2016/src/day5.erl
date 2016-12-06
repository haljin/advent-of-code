-module(day5).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve(Input) ->
  process_day4(Input, 1, []).

solve2(Input) ->
  Data = process_day4_2(Input, 1, []),
  {_Indices, Values} = lists:unzip(lists:keysort(1,Data)),
  print_hash(Values),
  Data.

process_day4(Input, N, Acc) when length(Acc) < 8 ->
  case erlang:md5(Input ++ integer_to_list(N)) of
    <<0, 0, ThirdByte, _Rest/binary>> when ThirdByte < 15 ->
      process_day4(Input, N + 1, Acc ++ [ThirdByte]);
    _ ->
      process_day4(Input, N + 1, Acc)
  end;
process_day4(_Input, _N, Acc) ->
  Acc.

process_day4_2(Input, N, Acc) when length(Acc) < 8 ->
  case erlang:md5(Input ++ integer_to_list(N)) of
    <<0, 0, ThirdByte, FourthDigit:4, _Rest/bits>> when ThirdByte < 15 ->
      process_day4_2(Input, N + 1, maybe_add_to_list(ThirdByte, FourthDigit, Acc));
    _ ->
      process_day4_2(Input, N + 1, Acc)
  end;
process_day4_2(_Input, _N, Acc) ->
  Acc.

maybe_add_to_list(Index, Number, Acc) when Index >= 0,
                                           Index =< 7 ->
  case lists:keymember(Index, 1, Acc) of
    true -> Acc;
    false ->
      [{Index, Number} | Acc]
  end;
maybe_add_to_list(_Index, _Number, Acc) ->
  Acc.


print_hash(Bin) ->
  [io:format("~1.16.0B", [Char]) || Char <- Bin].