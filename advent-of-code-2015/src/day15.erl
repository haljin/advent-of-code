-module(day15).

-compile(export_all).

-record(ingredient, {name = 0,
                     capacity = 0,
                     durability = 0,
                     flavor = 0,
                     texture = 0,
                     calories = 0}).

parse_data(File) ->
  santa_utils:map_file(File,
                       fun(Line, Acc) ->
                          [Name, "capacity", Capacity, "durability", Durability, "flavor",
                           Flavor, "texture", Texture, "calories", Calories] = string:tokens(Line -- "\n", ",: "),
                          [#ingredient{name = Name,
                                       capacity = list_to_integer(Capacity),
                                       durability = list_to_integer(Durability),
                                       flavor = list_to_integer(Flavor),
                                       texture = list_to_integer(Texture),
                                       calories = list_to_integer(Calories)} | Acc]
                        end, [read], []).

solve(Ingredients, Sum) ->
  Combos = combinations(length(Ingredients), Sum),
  Sums = sum_combos(Combos, Ingredients, []),
  R = lists:max(Sums),
  io:format("Result ~p~n", [R]).

solve2(Ingredients, Sum) ->
  Combos = combinations(length(Ingredients), Sum),
  Sums = sum_combos2(Combos, Ingredients, []),
  R = lists:max(Sums),
  io:format("Result ~p~n", [R]).

sum_combos([Combo | Rest], Ingredients, Acc) ->
  Score = sum_scores(lists:zip(Combo, Ingredients), #ingredient{}),
  sum_combos(Rest, Ingredients, [Score | Acc]);
sum_combos([], _, Acc) ->
  Acc.

sum_combos2([Combo | Rest], Ingredients, Acc) ->
  Score = sum_scores2(lists:zip(Combo, Ingredients), #ingredient{}),
  sum_combos2(Rest, Ingredients, [Score | Acc]);
sum_combos2([], _, Acc) ->
  Acc.


combinations(1, Sum) ->
    [[Sum]];
combinations(N, Sum) ->
  make_combos(Sum, N, Sum, []).

make_combos(Val, N, Sum, Res) when Val >= 0 ->
  Combos = [[Val | Combo] || Combo <- combinations(N-1, Sum - Val)],
  make_combos(Val - 1, N, Sum, Combos ++ Res);
make_combos(_, _, _, Res) ->
  Res.


sum_scores([{Count, #ingredient{capacity = C, durability = D, flavor = F, texture = T}}| Rest],
           Sum = #ingredient{capacity = SC, durability = SD, flavor = SF, texture = ST}) ->
  sum_scores(Rest, Sum#ingredient{capacity = SC + (C * Count),
                                  durability = SD + (D * Count),
                                  flavor =  SF + (F * Count),
                                  texture = ST + (T * Count)});
sum_scores([], #ingredient{capacity = SC, durability = SD, flavor = SF, texture = ST}) ->
  non_neg(SC) * non_neg(SD) * non_neg(SF) * non_neg(ST).


sum_scores2([{Count, #ingredient{capacity = C, durability = D, flavor = F, texture = T, calories = Cal}}| Rest],
           Sum = #ingredient{capacity = SC, durability = SD, flavor = SF, texture = ST, calories = SCal}) ->
  sum_scores2(Rest, Sum#ingredient{capacity = SC + (C * Count),
                                   durability = SD + (D * Count),
                                   flavor =  SF + (F * Count),
                                   texture = ST + (T * Count),
                                   calories = SCal + (Cal * Count)});
sum_scores2([], #ingredient{capacity = SC, durability = SD, flavor = SF, texture = ST, calories = 500}) ->
  non_neg(SC) * non_neg(SD) * non_neg(SF) * non_neg(ST);
sum_scores2([], _) ->
  0.


non_neg(Val) when Val < 0 ->
  0;
non_neg(Val) ->
  Val.