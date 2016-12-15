-module(day15).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(                     File, fun(X, Acc) ->
    [string:tokens(X, " .#;\n") | Acc] end, [read], []).

solve() ->
  Data = parse_data("../priv/day15input.txt"),
  Pids = spawn_discs(Data, []),
  register(control, self()),
  solve(Pids, 0).

solve2() ->
  Data = parse_data("../priv/day15input.txt"),
  ModifiedData = [["Disc", "7", "has", "11", "positions", "at", "time=0,", "it", "is", "at", "position", "0"] | Data],
  Pids = spawn_discs(ModifiedData, []),
  register(control, self()),
  solve(Pids, 0).

solve_alt() ->
  Processed = [{list_to_integer(StartPos), list_to_integer(Positions)} ||
                ["Disc", _Digit, "has", Positions, "positions", "at", "time=0,", "it", "is", "at", "position", StartPos] <- parse_data("../priv/day15input.txt")],
  solve_alt(lists:reverse(Processed), 0).

solve_alt2() ->
  Processed = [{list_to_integer(StartPos), list_to_integer(Positions)} ||
                ["Disc", _Digit, "has", Positions, "positions", "at", "time=0,", "it", "is", "at", "position", StartPos] <-
                  [["Disc", "7", "has", "11", "positions", "at", "time=0,", "it", "is", "at", "position", "0"] | parse_data("../priv/day15input.txt")]],
   solve_alt(lists:reverse(Processed), 0).


solve_alt(Discs, Time) ->
  case check_discs(increment_discs(Discs, [])) of
    true -> Time;
    false -> solve_alt(increment_discs(Discs, []), Time + 1)
  end.

check_discs([{0,_}]) ->
  true;
check_discs([{0, _} | Rest]) ->
  check_discs(increment_discs(Rest, []));
check_discs([{_NotZero, _} | _Rest]) ->
  false.

increment_discs([], Incremented) ->
  Incremented;
increment_discs([{DiscPos, DiscMax} | Rest], Incremented) when DiscPos =:= DiscMax - 1 ->
  increment_discs(Rest, Incremented ++ [{0, DiscMax}]);
increment_discs([{DiscPos, DiscMax} | Rest], Incremented) ->
  increment_discs(Rest, Incremented ++ [{DiscPos + 1, DiscMax}]).

solve([FirstDisc | _] = Pids, Time) ->
  io:format("TIME IS ~p~n", [Time]),
  FirstDisc ! {ball, Time, Time + 1},
  [Pid ! tick || Pid <- Pids],
  receive
    {ball, StartTime, _Time} ->
      [Pid ! die || Pid <- Pids],
      StartTime;
    ball_end_of_life ->
      solve(Pids, Time + 1)
  end.

disc_proc(MyId, Time, Positions, MyPosition, NextDisc) ->
  receive
    tick when MyPosition == Positions - 1 ->
%%    io:format("Disc ~p ticking over.~n", [MyId]),
      disc_proc(MyId, Time + 1, Positions, 0, NextDisc);
    tick ->
%%    io:format("Disc ~p ticking.~n", [MyId]),
      disc_proc(MyId, Time + 1, Positions, MyPosition + 1, NextDisc);
    {ball, StartTime, Time} when (MyPosition =:= 0) ->
%%    io:format("Disc ~p got ball ~p, letting it through! ~n", [MyId, {ball, StartTime, Time}]),
      NextDisc ! {ball, StartTime, Time + 1},
      control ! ball_end_of_life,
      disc_proc(MyId, Time, Positions, MyPosition, NextDisc);
    {ball, _BT, Time} ->
      control ! ball_end_of_life,
%%    io:format("Disc ~p got ball ~p, dropping it! ~n", [MyId, {ball, BT, Time}]),
      disc_proc(MyId, Time, Positions, MyPosition, NextDisc);
    die ->
      ok
  end.

spawn_discs([["Disc", Digit, "has", Positions, "positions", "at", "time=0,", "it", "is", "at", "position", StartPos] | Rest], PrevPids) ->
  NextDiscPid = case PrevPids of
                  [] -> self();
                  List -> hd(List)
                end,
  NewDisc = spawn(?MODULE, disc_proc, [list_to_integer(Digit), 0, list_to_integer(Positions), list_to_integer(StartPos), NextDiscPid]),
  spawn_discs(Rest, [NewDisc | PrevPids]);
spawn_discs([], Pids) ->
  Pids.


