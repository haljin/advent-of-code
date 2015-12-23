-module(day21).

-compile(export_all).
-record(stats, {name,
                hp = 0,
                damage = 0,
                armor = 0,
                items = []}).

-record(item, {name,
               cost = 0,
               damage = 0,
               armor = 0}).

-define(PLAYER, #stats{name = player, hp = 100, damage = 0, armor = 0}).

-define(WEAPONS, [#item{name = "Dagger", cost = 8, damage = 4, armor = 0},
                  #item{name = "Shortsword", cost = 10, damage = 5, armor = 0},
                  #item{name = "Warhammer", cost = 25, damage = 6, armor = 0},
                  #item{name = "Longsword", cost = 40, damage = 7, armor = 0},
                  #item{name = "Greataxe", cost = 74, damage = 8, armor = 0}]).

-define(ARMOR, [#item{name = "Leather", cost = 13, damage = 0, armor = 1},
                #item{name = "Chainmail", cost = 31, damage = 0, armor = 2},
                #item{name = "Splintmail", cost = 53, damage = 0, armor = 3},
                #item{name = "Bandedmail", cost = 75, damage = 0, armor = 4},
                #item{name = "Platemail", cost = 102, damage = 0, armor = 5}]).

-define(RINGS, [#item{name = "Damage +1", cost = 25, damage = 1, armor = 0},
                #item{name = "Damage +2", cost = 50, damage = 2, armor = 0},
                #item{name = "Damage +3", cost = 100, damage = 3, armor = 0},
                #item{name = "Defense +1", cost = 20, damage = 0, armor = 1},
                #item{name = "Defense +2", cost = 40, damage = 0, armor = 2},
                #item{name = "Defense +3", cost = 80, damage = 0, armor = 3}]).

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
  Equipments = generate_equipment_combos(?WEAPONS, ?ARMOR, ?RINGS),
  [spawn(?MODULE, game_proc, [Eq, Boss, self()]) || Eq <- Equipments],
  get_results(length(Equipments), []).

solve2(Boss) ->
  Equipments = generate_equipment_combos(?WEAPONS, ?ARMOR, ?RINGS),
  [spawn(?MODULE, rigged_game_proc, [Eq, Boss, self()]) || Eq <- Equipments],
  get_results(length(Equipments), []).

generate_equipment_combos(Weapons, Armor, Rings) ->
  RingCombos = ring_combos(Rings, Rings, []),
  [[W] || W <- Weapons] ++
  [[W, A] || W <- Weapons, A <- Armor] ++
  [[W] ++ R || W <- Weapons, R <- RingCombos] ++
  [[W, A] ++ R || W <- Weapons, A <- Armor, R <- RingCombos].

ring_combos([], Rings, Acc) ->
  SingleRings = [[Ring] || Ring <- Rings],
  [[]] ++ SingleRings ++ Acc;
ring_combos([Ring| Rest], Rings, Acc) ->
  NewCombos = [[Ring, AnotherRing] || AnotherRing <- Rings -- [Ring]],
  ring_combos(Rest, Rings, NewCombos ++ Acc).

get_results(0, Acc) ->
  lists:keysort(2, Acc);
get_results(N, Acc) ->
  receive
    lost -> get_results(N - 1, Acc);
    {win, Equipment, Cost} -> get_results(N - 1, [{Equipment, Cost}|Acc])
  end.

game_proc(Equipment, Boss, ResultPid) ->
  PlayerPid = spawn(?MODULE, character_proc, [?PLAYER, undefined, self()]),
  BossPid = spawn(?MODULE, character_proc, [Boss, PlayerPid, self()]),
  [PlayerPid ! {item, Item} || Item <- Equipment],
  PlayerPid ! {start, BossPid},
  receive
    {dead, player} ->
      BossPid ! die,
      ResultPid ! lost;
%%       io:format("Game over, player lost!~n");
    {dead, boss} ->
      PlayerPid ! die,
      Cost = lists:sum([C || #item{cost = C} <- Equipment]),
      ResultPid ! {win, Equipment, Cost}
%%       io:format("Game over, player won!~n")
  end.

rigged_game_proc(Equipment, Boss, ResultPid) ->
  PlayerPid = spawn(?MODULE, character_proc, [?PLAYER, undefined, self()]),
  BossPid = spawn(?MODULE, character_proc, [Boss, PlayerPid, self()]),
  [PlayerPid ! {item, Item} || Item <- Equipment],
  PlayerPid ! {start, BossPid},
  receive
    {dead, player} ->
      BossPid ! die,
      Cost = lists:sum([C || #item{cost = C} <- Equipment]),
      ResultPid ! {win, Equipment, Cost};
%%       io:format("Game over, player lost!~n");
    {dead, boss} ->
      PlayerPid ! die,
      ResultPid ! lost
%%       io:format("Game over, player won!~n")
  end.

character_proc(#stats{name = Name, hp = Hp, items = Items} = Me, Enemy, Game) ->
  receive
    {start, EnemyPid} ->
      EnemyPid ! {attack, calculate_damage(Me)},
      character_proc(Me, EnemyPid, Game);
    {item, Item} ->
      character_proc(Me#stats{items = [Item|Items]}, Enemy, Game);
    {attack, EnemyDmg} ->
      HpLost = case EnemyDmg - calculate_armor(Me) of
                 Val when Val =< 0 -> 1;
                 Val -> Val
               end,
%%       io:format("~p is hit for ~p damage.~n", [Name, HpLost]),
      case Hp - HpLost of
        Dead when Dead =< 0 ->
%%           io:format("~p has died~n", [Name]),
          Game ! {dead, Name};
        Alive ->
          Enemy ! {attack, calculate_damage(Me)},
          character_proc(Me#stats{hp = Alive}, Enemy, Game)
      end;
    die -> ok
  end.

calculate_damage(#stats{damage = Dmg, items = Items}) ->
  ItemDmg = [D || #item{damage = D} <- Items],
  lists:sum([Dmg| ItemDmg]).

calculate_armor(#stats{armor = AC, items = Items}) ->
  ItemAC = [A || #item{armor = A} <- Items],
  lists:sum([AC| ItemAC]).




