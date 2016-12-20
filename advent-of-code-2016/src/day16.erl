-module(day16).

-compile(export_all).


input() -> <<2#10001110011110000:17>>.


invert_bits(Binary) ->
  invert_bits(Binary, <<>>).

invert_bits(<<Bit:1, Rest/bitstring>>, Acc) ->
  invert_bits(Rest, <<bnot Bit:1, Acc/bitstring>>);
invert_bits(<<>>, Acc) ->
  Acc.

solve() ->
  solve(input(), 272).

solve2() ->
  solve(input(), 35651584).

solve(Binary, Length) when erlang:bit_size(Binary) < Length ->
  solve(<<Binary/bitstring, 0:1, (invert_bits(Binary))/bitstring>>, Length);
solve(Binary, Length) ->
  <<B:Length, _/bits>> = Binary,
  io:format("Binary is ~p~n", [print_bin(<<B:Length>>)]),
  print_bin(checksum(<<B:Length>>, <<>>)).


checksum(<<A:1, A:1, Rest/bits >>, Checksum) ->
  checksum(Rest, <<Checksum/bitstring, 1: 1>>);
checksum(<<_:1, _:1, Rest/bits >>, Checksum) ->
  checksum(Rest, <<Checksum/bitstring, 0: 1>>);
checksum(_, Checksum) when (bit_size(Checksum) rem 2) =:= 0 andalso bit_size(Checksum) > 0 ->
  checksum(Checksum, <<>>);
checksum(_, Checksum) ->
  Checksum.

print_bin(Binary) ->
  lists:flatten(io_lib:format("<<~s>>", [[io_lib:format("~1.2.0B",[X]) || <<X:1>> <= Binary ]])).


input_s() -> "10001110011110000".

invert_bits_s(Binary) ->
invert_bits_s(Binary, []).

invert_bits_s([$1 | Rest], Acc) ->
  invert_bits_s(Rest, [$0 | Acc]);
invert_bits_s([$0 | Rest], Acc) ->
  invert_bits_s(Rest, [$1 | Acc]);
invert_bits_s([], Acc) ->
  Acc.

solve_s() ->
  solve_s(input_s(), 272).

solve2_s() ->
  solve_s(input_s(), 35651584).

solve_s(Binary, Length) when length(Binary) < Length ->
  solve_s(Binary ++ [$0 | invert_bits_s(Binary)], Length);
solve_s(Binary, Length) ->
  checksum_s(lists:sublist(Binary, Length), []).


checksum_s([A, A | Rest], Checksum) ->
  checksum_s(Rest, [$1 | Checksum]);
checksum_s([_, _ | Rest], Checksum) ->
  checksum_s(Rest, [$0 | Checksum]);
checksum_s(_, Checksum) when (length(Checksum) rem 2 =:= 0) andalso length(Checksum) > 0 ->
  checksum_s(lists:reverse(Checksum), []);
checksum_s(_, Checksum) ->
  lists:reverse(Checksum).
