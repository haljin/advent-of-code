%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  @doc
%%  I solved this one before I figured out how to parallelize it. It generates
%%  a large "decision" tree with each of the spells that can be cast at any given
%%  point of time. It does so generating the tree dynamically while performing
%%  a DFS on it. Thanks to branch pruning this executes in ok time. The branches
%%  are pruned if:
%%  - The player dies
%%  - The boss dies
%%  - The player is out of mana
%%  - The game continues but the total cost exceeds the minimal solution that
%%    was already found before.
%%
%%  This COULD be parallelized. Instead of search through the tree in a single
%%  thread, each time new branches are generated, they could be spawned as
%%  sub-processes that would continue sub-processing. Pruning the 4th condition
%%  can become slightly tricky due to the asynchronous nature of such solutions.
%%  Not sure it is worth it that much.
%%
%%  @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

-record(node, {player, boss, summary_cost, spell, effects, chain = []}).

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


player_preturn(#node{player = Player, boss = Boss, effects = Effects, summary_cost = Total} = N) ->
  case {apply_effects(Player#stats{hp = Player#stats.hp + Player#stats.handicap}, Effects), apply_effects(Boss, Effects)} of
    {#stats{hp = PlayerHP}, _} when PlayerHP =< 0 ->
      lost;
    {_, #stats{hp = BossHP}} when BossHP =< 0 ->
      {win, Total};
    {PlayerTurnPlayer, PlayerTurnBoss} ->
      PlayerTurnEffects = [E#spell{duration = D - 1} || #spell{duration = D} = E <- Effects, D > 1],
      player_turn(N#node{player = PlayerTurnPlayer, boss = PlayerTurnBoss, effects = PlayerTurnEffects})
  end.

player_turn(#node{player = #stats{mana = Mana}, spell = #spell{cost = Cost}}) when Cost > Mana ->
  lost;
player_turn(#node{player = Player, boss = Boss, effects = Effects, spell = Spell, summary_cost = Total} = N) ->
  {EndPlayer, EndBoss, EndEffects} = apply_spell(Player, Boss, Effects, Spell),
  EndTotal = Total + Spell#spell.cost,
  case EndBoss#stats.hp of
    BossHp when BossHp =< 0 ->
      {win, EndTotal};
    _ ->
      boss_preturn(N#node{player = EndPlayer#stats{armor = 0}, boss = EndBoss, effects = EndEffects, summary_cost = EndTotal})
  end.

boss_preturn(#node{player = Player, boss = Boss, effects = Effects, summary_cost = Total} = N) ->
  case {apply_effects(Player, Effects), apply_effects(Boss, Effects)} of
    {_, #stats{hp = BossHP}} when BossHP =< 0 ->
      {win, Total};
    {BossTurnPlayer, BossTurnBoss} ->
      BossTurnEffects = [E#spell{duration = D - 1} || #spell{duration = D} = E <- Effects, D > 1],
      boss_turn(N#node{player = BossTurnPlayer, boss = BossTurnBoss, effects = BossTurnEffects})
  end.

boss_turn(#node{player = Player, boss = Boss} = N) ->
  EndPlayer = apply_boss_damage(Player, Boss),
  case EndPlayer#stats.hp of
    PlayerHp when PlayerHp =< 0 ->
      lost;
    _ ->
      {node, N#node{player = EndPlayer#stats{armor = 0}}}
  end.

game_tree([], _, CurrentBest) ->
  CurrentBest;
game_tree([#node{summary_cost = Cost} | Rest], AllSpells, CurrentBest) when (CurrentBest =/= undefined),
                                                                            (Cost >= CurrentBest)  ->
  game_tree(Rest, AllSpells, CurrentBest);
game_tree([#node{spell = OriginalSpell} = Node | Rest], AllSpells, CurrentBest) ->
  case player_preturn(Node) of
    lost ->
      game_tree(Rest, AllSpells, CurrentBest);
    {win, Total} ->
      io:format("A win ~p ~p ~p ~n", [Total, Node#node.chain, Node]),
      game_tree(Rest, AllSpells, find_best(CurrentBest, Total));
    {node, EndNode} ->
      PossibleSpells =  [E ||#spell{duration = D} = E <- EndNode#node.effects, D > 1],
      Branches = [EndNode#node{spell = Sp,
                               chain = [OriginalSpell#spell.name | EndNode#node.chain]}
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


