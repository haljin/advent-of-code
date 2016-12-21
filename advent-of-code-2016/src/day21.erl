-module(day21).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(                   File, fun(X, Acc) ->
    Acc ++ [string:tokens(X, " \n")] end, [read], []).

input() -> "abcdefgh".

input2() -> "fbgdceah".

solve() ->
  Data = parse_data("../priv/day21input.txt"),
  scramble(input(), Data).

solve2() ->
  Data = parse_data("../priv/day21input.txt"),
  unscramble(input2(), lists:reverse(Data)).

scramble(Input, []) ->
  Input;
scramble(Input, [["swap", "position", StringX, "with", "position", StringY] | Rest]) ->
  X = list_to_integer(StringX),
  Y = list_to_integer(StringY),
  scramble(swap_pos(Input, X, Y), Rest);
scramble(Input, [["swap", "letter", [Char1], "with", "letter", [Char2]] | Rest]) ->
  scramble(swap_let(Input, Char1, Char2), Rest);
scramble(Input, [["rotate", "left", StringX, _] | Rest]) ->
  X = list_to_integer(StringX),
  scramble(rot_left(Input, X), Rest);
scramble(Input, [["rotate", "right", StringX, _] | Rest]) ->
  X = list_to_integer(StringX),
  scramble(rot_right(Input, X), Rest);
scramble(Input, [["rotate", "based", "on", "position", "of", "letter", [Char]] | Rest]) ->
  scramble(rot_pos(Input, Char), Rest);
scramble(Input, [["reverse", "positions", StringX, "through", StringY] | Rest]) ->
  X = list_to_integer(StringX),
  Y = list_to_integer(StringY),
  scramble(rev(Input, X, Y), Rest);
scramble(Input, [["move", "position", StringX, "to", "position", StringY] | Rest]) ->
  X = list_to_integer(StringX),
  Y = list_to_integer(StringY),
  scramble(move(Input, X, Y), Rest).

unscramble(Input, []) ->
  Input;
unscramble(Input, [["swap", "position", StringX, "with", "position", StringY] | Rest]) ->
  X = list_to_integer(StringX),
  Y = list_to_integer(StringY),
  unscramble(swap_pos(Input, X, Y), Rest);
unscramble(Input, [["swap", "letter", [Char1], "with", "letter", [Char2]] | Rest]) ->
  unscramble(swap_let(Input, Char1, Char2), Rest);
unscramble(Input, [["rotate", "left", StringX, _] | Rest]) ->
  X = list_to_integer(StringX),
  unscramble(rot_right(Input, X), Rest);
unscramble(Input, [["rotate", "right", StringX, _] | Rest]) ->
  X = list_to_integer(StringX),
  unscramble(rot_left(Input, X), Rest);
unscramble(Input, [["rotate", "based", "on", "position", "of", "letter", [Char]] | Rest]) ->
  unscramble(rev_rot_pos(Input, Char), Rest);
unscramble(Input, [["reverse", "positions", StringX, "through", StringY] | Rest]) ->
  X = list_to_integer(StringX),
  Y = list_to_integer(StringY),
  unscramble(rev(Input, X, Y), Rest);
unscramble(Input, [["move", "position", StringX, "to", "position", StringY] | Rest]) ->
  X = list_to_integer(StringX),
  Y = list_to_integer(StringY),
  unscramble(move(Input, Y, X), Rest).


swap_pos(Input, X, Y) when X > Y ->
  swap_pos(0, Input, Y, X, []);
swap_pos(Input, X, Y) ->
  swap_pos(0, Input, X, Y, []).

swap_pos(X, [Char | Rest], X, Y, Acc) ->
  swap_pos2(X + 1, Rest, Y, Acc, [], Char);
swap_pos(N, [Char | Rest], X, Y, Acc) ->
  swap_pos(N + 1, Rest, X, Y, [Char | Acc]).

swap_pos2(Y, [Char | Rest], Y, Acc, Acc2, PrevChar) ->
  lists:reverse([PrevChar | Acc2] ++ [Char | Acc]) ++ Rest;
swap_pos2(N, [Char | Rest], Y, Acc, Acc2, PrevChar) ->
  swap_pos2(N + 1, Rest, Y, Acc, [Char | Acc2], PrevChar).

swap_let(Input, C1, C2) ->
  swap_let(Input, C1, C2, []).

swap_let([C1 | Rest], C1, C2, Acc) ->
  swap_let2(Rest, C1, C2, [C2 | Acc]);
swap_let([C2 | Rest], C1, C2, Acc) ->
  swap_let2(Rest, C2, C1, [C1 | Acc]);
swap_let([SomeC | Rest], C1, C2, Acc) ->
  swap_let(Rest, C1, C2, [SomeC | Acc]).

swap_let2([OtherC | Rest], CToReplace, OtherC, Acc) ->
  lists:reverse([CToReplace | Acc]) ++ Rest;
swap_let2([SomeC | Rest], CToReplace, OtherC, Acc) ->
  swap_let2(Rest, CToReplace, OtherC, [SomeC | Acc]).

rot_right(Input, N) ->
  lists:reverse(rot_left(lists:reverse(Input), N)).

rot_left(Input, 0) ->
  Input;
rot_left([Char | Input], N) ->
  rot_left(Input ++ [Char], N - 1).

rot_pos(Input, Char) ->
  rot_pos(Input, Char, Input, []).

rot_pos([Char | _Rest], Char, FullInput, Acc) when length(Acc) > 3 ->
  rot_right(FullInput, length(Acc) + 1 + 1);
rot_pos([Char | _Rest], Char, FullInput, Acc) ->
  rot_right(FullInput, length(Acc) + 1);
rot_pos([OtherChar | Rest], Char, FullInput, Acc) ->
  rot_pos(Rest, Char, FullInput, [OtherChar | Acc]).

rev(Input, X, Y) ->
  rev(0, Input, X, Y, []).

rev(X, [Char | Rest], X, Y, Acc) ->
  rev2(X + 1, Rest, Y, Acc, [Char]);
rev(N, [Char | Rest], X, Y, Acc) ->
  rev(N + 1, Rest, X, Y, [Char | Acc]).

rev2(Y, [Char | Rest], Y, Acc, Reversible) ->
  lists:reverse(Acc) ++ [Char | Reversible] ++ Rest;
rev2(N, [Char | Rest], Y, Acc, Reversible) ->
  rev2(N + 1, Rest, Y, Acc, [Char | Reversible]).

move(Input, X, Y) ->
  move(0, Input, X, Y, []).

move(X, [Char | Rest], X, Y, Acc) ->
  move2(0, lists:reverse(Acc) ++ Rest, Y, [], Char);
move(N, [Char | Rest], X, Y, Acc) ->
  move(N + 1, Rest, X, Y, [Char | Acc]).

move2(Y, Rest, Y, Acc, Char) ->
  lists:reverse([Char | Acc]) ++ Rest;
move2(N, [OtherChar | Rest], Y, Acc, Char) ->
  move2(N + 1, Rest, Y, [OtherChar | Acc], Char).

rev_rot_pos(Input, Char) ->
  Pos = find_char_pos(0, Input, Char),
  case Pos rem 2 of
    1 -> rot_left(Input, Pos div 2 + 1);
    0 when Pos =/= 0 -> rot_left(Input, (length(Input) + Pos) div 2 + 1);
    0 -> rot_left(Input, length(Input) + 1)
  end.


find_char_pos(N, [Char | _], Char) ->
  N;
find_char_pos(N, [_ | Rest], Char) ->
  find_char_pos(N + 1, Rest, Char).
