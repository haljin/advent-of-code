-module(day19).

-compile(export_all).

-define(IS_UPPER(X), (X >= 65 andalso X =< 90)).
-define(IS_LOWER(X), (X >= 97 andalso X =< 122)).

parse_data(File) ->
  santa_utils:map_file(File,
                       fun(Line, Acc) ->
                         case string:tokens(Line -- "\n", " =>") of
                           [Elem, Sequence] ->
                             [{list_to_atom(Elem), parse_sequence(Sequence, [])}|Acc];
                           [Sequence] ->
                             {parse_sequence(Sequence, []), Acc};
                           [] ->
                             Acc
                         end
                        end, [read], []).


parse_sequence([UpperCase, LowerCase | Rest], Acc) when ?IS_UPPER(UpperCase) andalso ?IS_LOWER(LowerCase) ->
  parse_sequence(Rest, [list_to_atom([UpperCase, LowerCase])| Acc]);
parse_sequence([UpperCase | Rest], Acc) when ?IS_UPPER(UpperCase) ->
  parse_sequence(Rest, [list_to_atom([UpperCase])| Acc]);
parse_sequence([], Acc) ->
  Acc.

solve({Sequence, Replacements}) ->
  Res = find_replacement([], Sequence, Replacements, []),
  length(lists:usort(Res)).

solve2({Sequence, Replacements}) ->
  SortedReps = lists:sort(fun({_,A}, {_,B}) -> length(A) >= length(B) end, Replacements),
  try_replacement(SortedReps, SortedReps, Sequence, 0).

try_replacement(_, _, [e], Count) ->
  Count;
try_replacement([{To, R} |Rest], AllReplacements, Sequence, Count) ->
  case replace([], R, Sequence, To, []) of
    {error, cannot_replace} ->
      try_replacement(Rest, AllReplacements, Sequence, Count);
    ReplacedSequence ->
      try_replacement(AllReplacements, AllReplacements, ReplacedSequence, Count + 1)
  end;
try_replacement([], _, Sequence, _) ->
  {error, Sequence}.


replace(Prefix, [Elem | Rest], [Elem | Rest2], Replacement, ReplacedSoFar) ->
  replace(Prefix, Rest, Rest2, Replacement, ReplacedSoFar ++ [Elem]);
replace(Prefix, [], Rest2, Replacement, _) ->
  Prefix ++ [Replacement] ++ Rest2;
replace(Prefix, NoMatch, [Elem| Rest2], Replacement, []) ->
  replace(Prefix ++ [Elem], NoMatch, Rest2, Replacement, []);
replace(Prefix, NoMatch, [Elem| Rest2], Replacement, ReplacedSoFar) ->
  replace(Prefix ++ ReplacedSoFar, ReplacedSoFar ++ NoMatch, [Elem | Rest2], Replacement, []);
replace(_Prefix, _, [], _FullReplacement, _) ->
  {error, cannot_replace}.
  
  

find_replacement(Prefix, [Atom | Rest], Replacements, Acc) ->
  Possibilites = [Prefix ++ R ++ Rest || {A, R} <- Replacements, A =:= Atom],
  find_replacement(Prefix ++ [Atom], Rest, Replacements, Possibilites ++ Acc);
find_replacement(_Prefix, [], _Replacements, Acc) ->
  Acc.