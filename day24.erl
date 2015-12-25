-module(day24).

-compile(export_all).

parse_data(File) ->
  lists:reverse(santa_utils:map_file(File,
    fun(Line, Acc) ->
            [list_to_integer(Line -- "\n") | Acc]
    end, [read], [])).

solve(Gifts) ->
  EqualSplit = lists:sum(Gifts) div 3,
  Results = fill(lists:reverse(Gifts), EqualSplit, []),
  Sorted = lists:sort(fun(A,B) -> length(A) < length(B) end, Results),
  Minimal = find_possible(Sorted, undefined, EqualSplit, Gifts, []),
  lists:min([lists:foldl(fun(El, Acc) -> El * Acc end, 1, Option) || Option <- Minimal]).

solve2(Gifts) ->
  EqualSplit = lists:sum(Gifts) div 4,
  Results = fill(lists:reverse(Gifts), EqualSplit, []),
  Sorted = lists:sort(fun(A,B) -> length(A) < length(B) end, Results),
  Minimal = find_possible(Sorted, undefined, EqualSplit, Gifts, []),
  lists:min([lists:foldl(fun(El, Acc) -> El * Acc end, 1, Option) || Option <- Minimal]).


get_results(0, Acc) ->
  Acc;
get_results(N, Acc) ->
  receive
    no_result ->
      get_results(N - 1, Acc);
    {result, R} ->
      get_results(N - 1, [R|Acc])
  end.


find_possible([Option|_], Length, _EqualSplit, _Gifts, Acc) when Length =/= undefined,
                                                                 length(Option) > Length ->
  Acc;
find_possible([Option|Rest], Length, EqualSplit, Gifts, Acc) ->
  case fill(Gifts -- Option, EqualSplit, []) of
    [] ->
      io:format("~p is not a possible load~n", [Option]),
      find_possible(Rest, Length, EqualSplit, Gifts, Acc);
    _Possible ->
      io:format("~p is possible load at length ~p~n", [Option, length(Option)]),
      find_possible(Rest, length(Option), EqualSplit, Gifts, [Option|Acc])
  end.

fill([], _Sum, Results) ->
  Results;
fill([Sum | T], Sum, Results) ->
  fill(T, Sum, [[Sum] | Results]);
fill([H|T], Sum, Results) when H < Sum ->
  SubResults = fill(T, Sum - H, []),
  FillResults = [[H| R] || R <- SubResults],
  fill(T, Sum, FillResults ++ Results);
fill([H|T], Sum, Results) when H > Sum ->
  fill(T, Sum, Results).


