-module(day5).

-compile(export_all).

parse_data(Path) ->
  santa_utils:map_file(Path, fun(X, Acc) ->
    Acc ++ [string:strip(X, both, $\n)]
                             end, [read], []).

solve(ListofStrings) ->
  process_day5(ListofStrings, 0).

process_day5([H | T], Acc) ->
  case string_eval(H, 0, false) of
    nice ->
      process_day5(T, Acc + 1);
    naughty ->
      process_day5(T, Acc)
  end;
process_day5([], Acc) ->
  Acc.

string_eval("ab" ++ _Rest, _Vowels, _Double) ->
  naughty;
string_eval("cd" ++ _Rest, _Vowels, _Double) ->
  naughty;
string_eval("pq" ++ _Rest, _Vowels, _Double) ->
  naughty;
string_eval("xy" ++ _Rest, _Vowels, _Double) ->
  naughty;
string_eval([Char1, Char1 | Rest], Vowels, _Double) ->
  NewVowels = case is_vowel(Char1) of
                true -> Vowels + 1;
                false -> Vowels
              end,
  string_eval([Char1 | Rest], NewVowels, true);
string_eval([Char | Rest], Vowels, Double) ->
  case is_vowel(Char) of
    true ->
      string_eval(Rest, Vowels + 1, Double);
    false ->
      string_eval(Rest, Vowels, Double)
  end;
string_eval([], Vowels, true) when Vowels >= 3 ->
  nice;
string_eval([], _, _) ->
  naughty.

solve2(ListofStrings) ->
  process_day5_2(ListofStrings, 0).

process_day5_2([H | T], Acc) ->
  case string_eval_2(H, #{}, false) of
    nice ->
      process_day5_2(T, Acc + 1);
    naughty ->
      process_day5_2(T, Acc)
  end;
process_day5_2([], Acc) ->
  Acc.

string_eval_2([Char1, Char1, Char1 | Rest], Pairs, _InBetween) ->
  string_eval_2([Char1 | Rest], pair_to_map([Char1, Char1], Pairs), true);
string_eval_2([Char1, Char2, Char1 | Rest], Pairs, _InBetween) ->
  string_eval_2([Char2, Char1 | Rest], pair_to_map([Char1, Char2], Pairs), true);
string_eval_2([Char1, Char2 | Rest], Pairs, InBetween) ->
  string_eval_2([Char2 | Rest], pair_to_map([Char1, Char2], Pairs), InBetween);
string_eval_2([_Char | Rest], Pairs, InBetween) ->
  string_eval_2(Rest, Pairs, InBetween);
string_eval_2([], Pairs, true) ->
  case lists:any(fun(V) -> V >= 2 end, maps:values(Pairs)) of
    true -> nice;
    false -> naughty
  end;
string_eval_2([], _, _) ->
  naughty.

pair_to_map(Pair, Map) ->
  case maps:find(Pair, Map) of
    {ok, Val} -> maps:put(Pair, Val + 1, Map);
    error -> maps:put(Pair, 1, Map)
  end.


is_vowel($a) -> true;
is_vowel($e) -> true;
is_vowel($i) -> true;
is_vowel($o) -> true;
is_vowel($u) -> true;
is_vowel(_)  -> false.