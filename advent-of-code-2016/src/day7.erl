-module(day7).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File, fun(X, Acc) ->
    Acc ++ [X -- "\n"] end, [read], []).

solve() ->
  Data = parse_data("../priv/day7input.txt"),
  solve(Data, []).

solve2() ->
  Data = parse_data("../priv/day7input.txt"),
  solve2(Data, 0).

solve([], Acc) ->
  length(Acc);
solve([String | Rest], Acc) ->
  solve(Rest, Acc ++ not_found(String, String)).

solve2([], Acc) ->
  Acc;
solve2([String | Rest], Acc) ->
  solve2(Rest, Acc + ssl_normal(String, [], [])).

not_found([A, B, B, A | Rest], Full) when A =/= B ->
  found(Rest, Full);
not_found([$[ | Rest], Full) ->
  in_brackets(Rest, Full);
not_found([_ | Rest], Full) ->
  not_found(Rest, Full);
not_found([], _Full) ->
  [].

found([$[ | Rest], Full) ->
  in_brackets_found(Rest, Full);
found([_ | Rest], Full) ->
  found(Rest, Full);
found([], Full) ->
  [Full].

in_brackets([A, B, B, A | _Rest], _Full) when A =/= B  ->
  [];
in_brackets([$] | Rest], Full) ->
  not_found(Rest, Full);
in_brackets([_| Rest], Full) ->
  in_brackets(Rest, Full);
in_brackets([], _Full) ->
  [].

in_brackets_found([A, B, B, A | _Rest], _Full) when A =/= B  ->
  [];
in_brackets_found([$] | Rest], Full) ->
  found(Rest, Full);
in_brackets_found([_| Rest], Full) ->
  in_brackets_found(Rest, Full);
in_brackets_found([], _Full) ->
  [].

ssl_normal([A, B, A | Rest], NormalSeqs, BracketSeqs) when A =/= B ->
  case seq_match([A,B,A], BracketSeqs) of
    true ->
      1;
    false ->
      ssl_normal([B, A | Rest], [[A, B, A] | NormalSeqs], BracketSeqs)
  end;
ssl_normal([$[ | Rest], NormalSeqs, BracketSeqs) ->
  ssl_bracket(Rest, NormalSeqs, BracketSeqs);
ssl_normal([_ | Rest], NormalSeqs, BracketSeqs) ->
  ssl_normal(Rest, NormalSeqs, BracketSeqs);
ssl_normal([], _, _) ->
  0.

ssl_bracket([A, B, A | Rest], NormalSeqs, BracketSeqs) when A =/= B ->
  case seq_match([A,B,A], NormalSeqs) of
    true ->
      1;
    false ->
      ssl_bracket([B, A | Rest], NormalSeqs, [[A, B, A] | BracketSeqs])
  end;
ssl_bracket([$] | Rest], NormalSeqs, BracketSeqs) ->
  ssl_normal(Rest, NormalSeqs, BracketSeqs);
ssl_bracket([_ | Rest], NormalSeqs, BracketSeqs) ->
  ssl_bracket(Rest, NormalSeqs, BracketSeqs);
ssl_bracket([], _, _) ->
  0.

seq_match([A, B, A], [[B, A, B] | _Rest]) when A =/= B ->
  true;
seq_match(Seq, [_ | Rest]) ->
  seq_match(Seq, Rest);
seq_match(_, []) ->
  false.