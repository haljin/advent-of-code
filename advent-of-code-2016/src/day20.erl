-module(day20).

-compile(export_all).
-define(MAXIP, 4294967295).

parse_data(File) ->
  santa_utils:map_file(                                   File, fun(X, Acc) ->
    [A, B] = string:tokens(X, "-\n"),
    [{list_to_integer(A), list_to_integer(B)} | Acc] end, [read], []).

solve() ->
  Data = parse_data("../priv/day20input.txt"),
  Merged = merge_it(lists:usort(Data), [], length(Data)),
  {_FirstRangeStart, FirstRangeEnd} = hd(Merged),
  FirstRangeEnd + 1.

solve2() ->
  Data = parse_data("../priv/day20input.txt"),
  Merged = merge_it(lists:usort(Data), [], length(Data)),
  Inverted = invert_ranges(Merged),
  sum_ranges(Inverted, 0).


merge_it([Range1, Range2 | Rest], Acc, PrevLength) ->
  case merge_ranges(Range1, Range2) of
    no_merge ->
      merge_it([Range2 | Rest], [Range1 | Acc], PrevLength);
    NewRange ->
      merge_it(Rest, [NewRange | Acc], PrevLength)
  end;
merge_it([Odd], Acc, PrevLength) ->
  merge_it([], [Odd | Acc], PrevLength);
merge_it([], Acc, PrevLength) when length(Acc) =:= PrevLength ->
  lists:usort(Acc);
merge_it([], Acc, _) ->
  merge_it(lists:usort(Acc), [], length(Acc)).

merge_ranges({A1, B1}, {A2, B2}) when A1 >= A2 andalso A1 =< B2;
                                      B1 >= A2 andalso B1 =< B2;
                                      A1 =< A2 andalso B1 >= B2;
                                      A2 =< A1 andalso B2 >= B1 ->
  {min(A1, A2), max(B1, B2)};
merge_ranges({A1, B1}, {A2, B2}) when A2 =:= B1 + 1;
                                      A1 =:= B2 + 1 ->
  {min(A1, A2), max(B1, B2)};
merge_ranges(_, _) ->
  no_merge.

invert_ranges(Merged) ->
  First = maybe_add_first(Merged),
  First ++ invert_ranges(Merged, []).

invert_ranges([{_A1, B1}, {A2, B2} | Rest], Acc) ->
  invert_ranges([{A2, B2} | Rest], [{B1 + 1, A2 - 1}| Acc]);
invert_ranges([{_A, B}], Acc) when B < ?MAXIP->
  invert_ranges([], [{B + 1, ?MAXIP} | Acc]);
invert_ranges([_], Acc) ->
  invert_ranges([], Acc);
invert_ranges([], Acc) ->
  lists:sort(Acc).

maybe_add_first([{A, _} | _]) when A > 0 ->
  [{0, A -1}];
maybe_add_first(_) ->
  [].

sum_ranges([{A, B} | Rest], Acc) ->
  sum_ranges(Rest, Acc + (B - A) + 1);
sum_ranges(_, Acc) ->
  Acc.