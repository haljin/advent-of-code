-module(day22).

-compile(export_all).

-record(stats, {name,
                hp = 0,
                damage = 0,
                armor = 0,
                mana = 0,
                handicap = 0}).

-record(spell, {name,
                cost = 0,
                duration,
                targets,
                effect,
                value}).

-record(node, {turn = player, player, boss, summary_cost, spell, effects, chain = []}).

-define(SPELLS, [#spell{name = "Magic Missile", cost = 53, effect = damage, value = 4},
                 #spell{name = "Drain", cost =  73, effect = drain, value = 2},
                 #spell{name = "Shield", cost =  113, duration = 6, effect = armor, value = 7},
                 #spell{name = "Poison", cost =  173, duration = 6, effect = damage, value = 3},
                 #spell{name = "Recharge", cost = 229, duration = 5, effect = mana, value = 101}]).

-define(PLAYER, #stats{name = player, hp = 50, mana = 500}).
-define(PLAYER_HARD, #stats{name = player, hp = 50, mana = 500, handicap = -1}).

parse_data(File) ->
  santa_utils:map_file(File,
                       fun(Line, Acc) ->
                         case string:tokens(Line -- "\n", " :") of
                           ["Hit", "Points", HP] ->
                             Acc#stats{hp = list_to_integer(HP)};
                           ["Damage", Damage] ->
                             Acc#stats{damage = list_to_integer(Damage)};
                           ["Armor", Armor] ->
                             Acc#stats{armor = list_to_integer(Armor)}
                         end
                       end, [read], #stats{name = boss}).

solve(Boss) ->
  Nodes = [#node{player = ?PLAYER, boss = Boss, spell = Spell, effects = [], summary_cost = 0} || Spell <- ?SPELLS],
  game_tree(Nodes, ?SPELLS, undefined).

solve2(Boss) ->
  Nodes = [#node{player = ?PLAYER_HARD, boss = Boss, spell = Spell, effects = [], summary_cost = 0} || Spell <- ?SPELLS],
  game_tree(Nodes, ?SPELLS, undefined).

game_tree([], _, CurrentBest) ->
  io:format("Best: ~p~n", [CurrentBest]);
game_tree([#node{turn = player,
                 summary_cost = Cost} | Rest], AllSpells, CurrentBest) when (CurrentBest =/= undefined),
                                                                            (Cost >= CurrentBest)  ->
  game_tree(Rest, AllSpells, CurrentBest);
game_tree([#node{turn = player,
                 player = #stats {hp = Hp, handicap = Handicap} = Player,
                 boss = Boss,
                 spell = Spell,
                 effects = Effects,
                 summary_cost = Total} = N | Rest], AllSpells, CurrentBest) ->
 case {apply_effects(Player#stats{hp = Hp + Handicap}, Effects), apply_effects(Boss, Effects)} of
   {#stats{hp = PlayerHP}, _} when PlayerHP =< 0 ->
     game_tree(Rest, AllSpells, CurrentBest);
   {_, #stats{hp = BossHP}} when BossHP =< 0 ->
     io:format("Found a win ~p with ~p ~n", [Total, N#node.chain]),
     game_tree(Rest, AllSpells, find_best(CurrentBest, Total));
   {#stats{mana = M}, _} when M < Spell#spell.cost ->
     game_tree(Rest, AllSpells, CurrentBest);
   {PlayerTurnPlayer, PlayerTurnBoss} ->
     PlayerTurnEffects = [E#spell{duration = D - 1} || #spell{duration = D} = E <- Effects, D > 1],
     {EndPlayer, EndBoss, EndEffects} = apply_spell(PlayerTurnPlayer, PlayerTurnBoss, PlayerTurnEffects, Spell),
     EndTotal = Total + Spell#spell.cost,
     case EndBoss#stats.hp of
       BossHp when BossHp =< 0 ->
         io:format("Found a win ~p with ~p ~n", [EndTotal, [Spell#spell.name|N#node.chain]]),
         game_tree(Rest, AllSpells, find_best(CurrentBest, EndTotal));
       _ ->
         game_tree([N#node{turn = boss,
                           player = EndPlayer#stats{armor = 0},
                           boss = EndBoss,
                           effects = EndEffects,
                           chain = [Spell#spell.name|N#node.chain],
                           summary_cost = EndTotal} | Rest], AllSpells, CurrentBest)
     end
 end;
game_tree([#node{turn = boss,
                 player = Player,
                 boss = Boss,
                 effects = Effects,
                 summary_cost = Total} = N | Rest], AllSpells, CurrentBest) ->
  BossTurnPlayer = apply_effects(Player, Effects),
  BossTurnBoss = apply_effects(Boss, Effects),
  BossTurnEffects = [E#spell{duration = D - 1} || #spell{duration = D} = E <- Effects, D > 1],
  EndPlayer = apply_boss_damage(BossTurnPlayer, BossTurnBoss),
  case {EndPlayer#stats.hp, BossTurnBoss#stats.hp} of
    {_, BossHp} when BossHp =< 0 ->
      io:format("Found a win ~p with ~p ~n", [Total, N#node.chain]),
      game_tree(Rest, AllSpells, find_best(CurrentBest, Total));
    {PlayerHp, _} when PlayerHp =< 0 ->
      game_tree(Rest, AllSpells, CurrentBest);
    {_PlayerHp, _BossHp} ->
      PossibleSpells =  [E ||#spell{duration = D} = E <- BossTurnEffects, D > 1],
      Branches = [#node{spell = Sp,
                        player = EndPlayer#stats{armor = 0},
                        boss = BossTurnBoss,
                        effects = BossTurnEffects,
                        summary_cost = Total,
                        chain = N#node.chain}
                  || #spell{name = SN} = Sp <- AllSpells, not lists:keymember(SN, #spell.name, PossibleSpells)],
      game_tree(Branches ++ Rest, AllSpells, CurrentBest)
  end.

apply_spell(#stats{mana = Mana} = Player, #stats{} = Boss, Effects,
            #spell{effect = damage, duration = D, cost = Cost} = E) when D =/= undefined ->
  {Player#stats{mana = Mana - Cost}, Boss, [E#spell{targets = boss} | Effects]};
apply_spell(#stats{mana = Mana} = Player, #stats{} = Boss, Effects,
            #spell{effect = _Other, duration = D, cost = Cost} = E) when D =/= undefined ->
  {Player#stats{mana = Mana - Cost}, Boss, [E#spell{targets = player} | Effects]};
apply_spell(#stats{mana = Mana} = Player, #stats{hp = Hp} = Boss, Effects,
            #spell{effect = damage, value = Val, cost = Cost}) ->
  {Player#stats{mana = Mana - Cost}, Boss#stats{hp = Hp - Val}, Effects};
apply_spell(#stats{hp = PHp, mana = Mana} = Player, #stats{hp = Hp} = Boss, Effects,
            #spell{effect = drain, value = Val, cost = Cost}) ->
  {Player#stats{hp = PHp + Val, mana = Mana - Cost}, Boss#stats{hp = Hp - Val}, Effects}.

apply_boss_damage(Player, Boss) ->
  Player#stats{hp = Player#stats.hp - apply_armor(Boss#stats.damage, Player#stats.armor)}.

apply_effects(#stats{name = Target, armor = A} = Char, [#spell{targets = Target, effect = armor, value = Val}| Rest]) ->
  apply_effects(Char#stats{armor = A + Val}, Rest);
apply_effects(#stats{name = Target, hp = Hp} = Char, [#spell{targets = Target, effect = damage, value = Val}| Rest]) ->
  apply_effects(Char#stats{hp = Hp - Val}, Rest);
apply_effects(#stats{name = Target, mana = M} = Char, [#spell{targets = Target, effect = mana, value = Val}| Rest]) ->
  apply_effects(Char#stats{mana = M + Val}, Rest);
apply_effects(Char, [_ | Rest]) ->
  apply_effects(Char, Rest);
apply_effects(Char, []) ->
  Char.


apply_armor(Damage, Armor) ->
  case Damage - Armor of
    Val when Val =< 0 -> 1;
    Val -> Val
  end.

find_best(undefined, NewBest) ->
  NewBest;
find_best(CurrentBest, NewBest) when CurrentBest =< NewBest ->
  CurrentBest;
find_best(_, NewBest) ->
  NewBest.


