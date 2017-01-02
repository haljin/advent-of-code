-module(day24).
-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File,
                       fun(X, Acc) -> Acc ++ [X -- "\n"] end, [read], []).

solve() ->
  Data = parse_data("../priv/day24input.txt"),
  {Width, Height} = {length(hd(Data)), length(Data)},
  POIs = create_area(0, 0, Data, []),
  CleanPOIs = [POI || {POI, _} <- POIs, POI =/= '0'],
  Distances = find_distances(POIs, Width, Height, []),
  Perms = [['0' | Rest] || Rest <- santa_utils:permutations(CleanPOIs)],
  min_distance(Perms, Distances, undefined, undefined).


solve2() ->
  Data = parse_data("../priv/day24input.txt"),
  {Width, Height} = {length(hd(Data)), length(Data)},
  POIs = create_area(0, 0, Data, []),
  CleanPOIs = [POI || {POI, _} <- POIs, POI =/= '0'],
  Distances = find_distances(POIs, Width, Height, []),
  Perms = [['0' | Rest] ++ ['0'] || Rest <- santa_utils:permutations(CleanPOIs)],
  min_distance(Perms, Distances, undefined, undefined).

create_area(X, Y, [[$# | RowRest] | AreaRest], Acc) ->
  register(proc_name(X, Y), spawn(?MODULE, wall_proc, [])),
  create_area(X + 1, Y, [RowRest | AreaRest], Acc);
create_area(X, Y, [[$. | RowRest] | AreaRest], Acc) ->
  register(proc_name(X, Y), spawn(?MODULE, area_proc, [X, Y, undefined])),
  create_area(X + 1, Y, [RowRest | AreaRest], Acc);
create_area(X, Y, [[Char | RowRest] | AreaRest], Acc) when Char >= $0,
                                                           Char =< $9 ->
  Pid = spawn(?MODULE, area_proc, [X, Y, undefined]),
  register(proc_name(X, Y), Pid),
  create_area(X + 1, Y, [RowRest | AreaRest], [{list_to_atom([Char]), Pid} | Acc]);
create_area(_X, Y, [[] | AreaRest], Acc) ->
  create_area(0, Y + 1, AreaRest, Acc);
create_area(_, _, [], Acc) ->
  Acc.

find_distances([{POI, Pid} | Rest], Width, Height, Acc) ->
  Pid ! {flow, 0},
  timer:sleep(1000), %% A very very cheap trick :(
  Results = collect_results(Rest, []),
  [proc_name(X, Y) ! reset || X <- lists:seq(0, Width - 1), Y <- lists:seq(0, Height - 1)],
  New = [{POI, OtherPOI, Distance} || {OtherPOI, Distance} <- Results],
  find_distances(Rest, Width, Height, Acc ++ New);
find_distances([], Width, Height, Acc) ->
  [proc_name(X, Y) ! die || X <- lists:seq(0, Width - 1), Y <- lists:seq(0, Height - 1)],
  Acc.

collect_results([], Acc) ->
  Acc;
collect_results([{POI, Pid} | Rest], Acc) ->
  Pid ! {get, self()},
  receive
    {value, Val} ->
      collect_results(Rest, [{POI, Val} | Acc])
  end.

min_distance([Permutation | Rest], DistanceTable, Min, MinPermutation) ->
  NewDist = sum_distance(Permutation, 0, DistanceTable),
  case NewDist of
    Val when Min =:= undefined ->
      min_distance(Rest, DistanceTable, Val, Permutation);
    Val when Val < Min ->
      min_distance(Rest, DistanceTable, Val, Permutation);
    _ ->
      min_distance(Rest, DistanceTable, Min, MinPermutation)
  end;
min_distance([], _DistanceTable, Min, MinPermutation) ->
  {Min, MinPermutation}.

sum_distance([_], Sum, _) ->
  Sum;
sum_distance([X, Y | Rest], Sum, DistanceTable) ->
  [Dis] = [D || {POIA, POIB, D} <- DistanceTable, (POIA =:= X andalso POIB =:= Y) orelse
                                                  (POIA =:= Y andalso POIB =:= X)],
  sum_distance([Y | Rest], Sum + Dis, DistanceTable).

wall_proc() ->
  receive
    die -> ok;
    _ -> wall_proc()
  end.

area_proc(X, Y, N) ->
  receive
    die -> ok;
    {get, ResPid} ->
      ResPid ! {value, N},
      area_proc(X, Y, undefined);
    {flow, NewN} when N =:= undefined;
                      NewN < N ->
      flow_to_neighbours(X, Y, NewN),
      area_proc(X, Y, NewN);
    {flow, _} ->
      area_proc(X, Y, N);
    reset ->
      area_proc(X, Y, undefined)
  end.

flow_to_neighbours(X, Y, N) ->
  Neighbours = [proc_name(NX, NY) || {NX, NY} <- [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}]],
  [case whereis(Neighbour) of
     undefined -> skip;
     Pid -> Pid ! {flow, N + 1}
   end || Neighbour <- Neighbours].

proc_name(X, Y) ->
  list_to_atom("area_" ++ integer_to_list(X) ++ "_" ++ integer_to_list(Y)).