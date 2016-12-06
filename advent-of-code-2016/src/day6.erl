-module(day6).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File, fun(X, Acc) ->
    Acc ++ [X -- "\n"] end, [read], []).

solve() ->
  Data = parse_data("../priv/day6input.txt"),
  Pids = spawn_workers(hd(Data)),
  solve(Data, Pids, most).

solve2() ->
  Data = parse_data("../priv/day6input.txt"),
  Pids = spawn_workers(hd(Data)),
  solve(Data, Pids, least).

test_solve() ->
  Data = ["eedadn",
  "drvtee",
  "eandsr",
  "raavrd",
  "atevrs",
  "tsrnev",
  "sdttsa",
  "rasrtv",
  "nssdts",
  "ntnada",
  "svetve",
  "tesnvt",
  "vntsnd",
  "vrdear",
  "dvrsen",
  "enarar"],
  Pids = spawn_workers(hd(Data)),
  solve(Data, Pids, most).

solve([], Pids, Mode) ->
  [get_value(Pid, Mode) || Pid <- Pids];
solve([String | Rest], Pids, Type) ->
  [Pid ! {count, Char} || {Char, Pid} <- lists:zip(String, Pids)],
  solve(Rest, Pids, Type).



spawn_workers(Args) ->
  [spawn(?MODULE, count, [[]]) || _ <- Args].

count(Data) ->
  receive
    {count, Char} ->
      case lists:keyfind(Char, 1, Data) of
        {Char, Count} -> count(lists:keyreplace(Char, 1, Data, {Char, Count+1}));
        false -> count([{Char, 1}| Data])
      end;
    {most, Pid} ->
      Pid ! hd(lists:reverse(lists:keysort(2, Data)));
    {least, Pid} ->
      Pid ! hd(lists:keysort(2, Data))
  end.

get_value(Pid, Mode) ->
  Pid ! {Mode, self()},
  receive
    {Char, _Count} -> Char
  end.