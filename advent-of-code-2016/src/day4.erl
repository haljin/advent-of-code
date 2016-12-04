-module(day4).

-compile(export_all).
-record(room, {name,
               sector,
               checksum}).

parse_data(File) ->
  santa_utils:map_file(File, fun(X, Acc) ->
    [RoomAndSector, Checksum] = string:tokens(X -- "\n", "[]"),
    [Sector | ReverseRoom] = lists:reverse(string:tokens(RoomAndSector, "-")),
    Acc ++ [#room{name = lists:reverse(ReverseRoom), sector = list_to_integer(Sector), checksum = Checksum}]
                             end, [read], []).


solve() ->
  solve(parse_data("../priv/day4input.txt"), 0).

solve2() ->
  solve2(parse_data("../priv/day4input.txt"), []).

solve([], SectorSum) ->
  SectorSum;
solve([Room | Rest], SectorSum) ->
  case is_real_room(Room) of
    true -> solve(Rest, SectorSum + Room#room.sector);
    false -> solve(Rest, SectorSum)
  end.

solve2([], Rooms) ->
  find_north_pole(Rooms);
solve2([Room | Rest], Rooms) ->
  case is_real_room(Room) of
    true -> solve2(Rest, Rooms ++ [{decrypt(link_name(Room#room.name, []), [], Room#room.sector), Room#room.sector}]);
    false -> solve2(Rest, Rooms)
  end.

find_north_pole([]) ->
  {error, not_found};
find_north_pole([{Room, Sector}| Rest]) ->
  Words = string:tokens(Room, " "),
  case lists:member("northpole", Words) of
    true -> {Room, Sector};
    false -> find_north_pole(Rest)
  end.

is_real_room(#room{name = Name, checksum = Checksum}) ->
  calc_checksum(lists:flatten(Name), []) == Checksum.

calc_checksum([], Acc) ->
  SortedList = lists:sort(fun({L1, C}, {L2, C}) ->
                              L1 =< L2;
                             ({_L1, C1}, {_L2, C2}) ->
                               C1 >= C2
                          end, Acc),
  {CheckSum, _Counts} = lists:unzip(SortedList),
  lists:sublist(CheckSum, 5);
calc_checksum([Letter| Rest], Acc) ->
  case lists:keyfind(Letter, 1, Acc) of
    {Letter, Count} ->
      calc_checksum(Rest, lists:keyreplace(Letter, 1, Acc, {Letter, Count + 1}));
    false ->
      calc_checksum(Rest, [{Letter, 1} | Acc])
  end.

link_name([], Acc) ->
  Acc;
link_name([Part | Rest], Acc) ->
  link_name(Rest, Acc ++ "-" ++ Part).

decrypt([], Acc, _Shift) ->
  lists:reverse(Acc);
decrypt([Letter | Rest], Acc, Shift) ->
  decrypt(Rest, [shift(Letter, Shift) | Acc], Shift).


shift(Letter, 0) ->
  Letter;
shift($-, _Shift) ->
  $ ;
shift($z, Shift) ->
  shift($a, Shift - 1);
shift(Letter, Shift) ->
  shift(Letter + 1, Shift - 1).
