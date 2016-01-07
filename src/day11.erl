-module(day11).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve(InputString) ->
  case process_day11(InputString, false, []) of
    bad_password ->
      solve(increment_password(InputString));
    ok ->
      io:format("Result: ~p~n", [InputString]),
      InputString
  end.

solve2(InputString) ->
  solve(increment_password(solve(InputString))).

process_day11([BadChar | _T], _Sequence, _Pairs) when 	BadChar =:= $i;
                                                       BadChar =:= $o;
                                                       BadChar =:= $l ->
  bad_password;
process_day11([Char1, Char2, Char3 | T], _Sequence, Pairs) when Char2 =:= Char1 + 1,
                                                                Char3 =:= Char2 + 1 ->
  process_day11([Char2, Char3 | T], true, Pairs);
process_day11([Char1, Char1 | T], Sequence, Pairs) ->
  process_day11([Char1 | T], Sequence, [Char1 | Pairs]);
process_day11([_Char | T], Sequence, Pairs) ->
  process_day11(T, Sequence, Pairs);
process_day11([], Sequence, Pairs) ->
  case Sequence andalso length(lists:usort(Pairs)) >= 2 of
    true -> ok;
    false -> bad_password
  end.

increment_password(Password) ->
  RevPass = lists:reverse(Password),
  increment_password(RevPass, [], 1).


increment_password([$h | T], Acc, 1) ->
  increment_password(T, [$j | Acc], 0);
increment_password([$n | T], Acc, 1) ->
  increment_password(T, [$p | Acc], 0);
increment_password([$k | T], Acc, 1) ->
  increment_password(T, [$m | Acc], 0);
increment_password([$z | T], Acc, 1) ->
  increment_password(T, [$a | Acc], 1);
increment_password([Other | T], Acc, Shift) ->
  increment_password(T, [Other + Shift | Acc], 0);
increment_password([], Acc, _) ->
  Acc.