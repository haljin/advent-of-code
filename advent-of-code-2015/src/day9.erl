-module(day9).

-compile(export_all).

parse_data(Path) ->
  santa_utils:map_file(Path, fun(X, Acc) ->
    Entry = string:tokens(string:strip(X, both, $\n), " "),
    Acc ++ [Entry]
                              end, [read], []).

solve(ListofStrings) ->
  {AllLocs, Paths} = preparse_day9(ListofStrings, [], []),
  AllPaths = process_day9([[L] || L<- AllLocs], AllLocs, Paths, []),
  hd(lists:keysort(2, AllPaths)).

solve2(ListofStrings) ->
  {AllLocs, Paths} = preparse_day9(ListofStrings, [], []),
  AllPaths = process_day9([[L] || L<- AllLocs], AllLocs, Paths, []),
  lists:last(lists:keysort(2, AllPaths)).

preparse_day9([[From, "to", To, "=", Length] | Tail], AllLocs, Parsed) ->
  ParsedFrom = list_to_atom(From), ParsedTo = list_to_atom(To), ParsedLength = list_to_integer(Length),
  preparse_day9(Tail, [ParsedFrom, ParsedTo | AllLocs], [{ParsedFrom, ParsedTo, ParsedLength} | Parsed]);
preparse_day9([], AllLocs, Parsed) ->
  {lists:usort(AllLocs), Parsed}.

process_day9([CurPath | RestPaths], AllLocs, ValidPaths, FinishedPaths) ->
  case valid_paths(hd(CurPath), ValidPaths) -- CurPath of
    [] ->
      process_day9(RestPaths, AllLocs, ValidPaths, [{CurPath, calc_length(CurPath, ValidPaths, 0)} | FinishedPaths]);
    ValidLocs ->
      NewPaths = [[NewLoc | CurPath] || NewLoc <- ValidLocs -- CurPath],
      process_day9(NewPaths ++ RestPaths, AllLocs, ValidPaths, FinishedPaths)
  end;
process_day9([], _, _, FinishedPaths) ->
  FinishedPaths.

calc_length([Loc1, Loc2 | Rest], AllPaths, Acc) ->
  Length = find_path(Loc1, Loc2, AllPaths),
  calc_length([Loc2| Rest], AllPaths, Acc + Length);
calc_length([_SingleLoc], _, Acc) ->
  Acc.

find_path(From, To, Paths) ->
  [Length] = [L || {F, T, L} <- Paths, F =:= From andalso T =:= To]
             ++ [L || {F, T, L} <- Paths, T =:= From andalso F =:= To],
  Length.


valid_paths(From, Paths) ->
  [T || {F, T, _} <- Paths, F =:= From] ++ [F || {F, T, _} <- Paths, T =:= From].