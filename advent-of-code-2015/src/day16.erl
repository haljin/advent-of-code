-module(day16).

-compile(export_all).
-define(TICKER, [{children, 3},
                 {cats, 7},
                 {samoyeds, 2},
                 {pomeranians, 3},
                 {akitas, 0},
                 {vizslas, 0},
                 {goldfish, 5},
                 {trees, 3},
                 {cars, 2},
                 {perfumes, 1}]).

parse_data(File) ->
  santa_utils:map_file(File,
                       fun(Line, Acc) ->
                          ["Sue", Index | Rest] = string:tokens(Line -- "\n", ",: "),
                          [ {list_to_integer(Index), things_to_proplist(Rest, [])} | Acc]
                        end, [read], []).

things_to_proplist([Key, Val| Rest], Acc) ->
  things_to_proplist(Rest, [{list_to_atom(Key), list_to_integer(Val)} | Acc]);
things_to_proplist([], Acc) ->
  Acc.

solve(Sues) ->
  find_sue(Sues, ?TICKER).

solve2(Sues) ->
  find_sue2(Sues, ?TICKER).

find_sue([{SueIndex, Data} | Rest], RealSue) ->
  case validate_prop(Data, RealSue) of
    false -> find_sue(Rest, RealSue);
    true -> SueIndex
  end;
find_sue([], _) ->
  {error, no_sue}.

find_sue2([{SueIndex, Data} | Rest], RealSue) ->
  case validate_prop2(Data, RealSue) of
    false -> find_sue2(Rest, RealSue);
    true -> SueIndex
  end;
find_sue2([], _) ->
  {error, no_sue}.


validate_prop([{Prop, Val} | Rest], SuesProps) ->
  case proplists:get_value(Prop, SuesProps) of
    Val -> validate_prop(Rest, SuesProps);
    _ -> false
  end;
validate_prop([], _) ->
  true.


validate_prop2([{MoreThanProp, Val} | Rest], SuesProps) when MoreThanProp =:= cats;
                                                             MoreThanProp =:= trees ->
  case proplists:get_value(MoreThanProp, SuesProps) of
    Val2 when Val2 < Val -> validate_prop2(Rest, SuesProps);
    _ -> false
  end;
validate_prop2([{LessThanProp, Val} | Rest], SuesProps) when LessThanProp =:= pomeranians;
                                                             LessThanProp =:= goldfish ->
  case proplists:get_value(LessThanProp, SuesProps) of
    Val2 when Val2 > Val -> validate_prop2(Rest, SuesProps);
    _ -> false
  end;
validate_prop2([{Prop, Val} | Rest], SuesProps) ->
  case proplists:get_value(Prop, SuesProps) of
    Val -> validate_prop2(Rest, SuesProps);
    _ -> false
  end;
validate_prop2([], _) ->
  true.
