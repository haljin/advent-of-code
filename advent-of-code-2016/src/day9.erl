-module(day9).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve() ->
  Data = parse_data("../priv/day9input.txt"),
  Decompressed = decompress(Data),
  length(Decompressed).

solve2() ->
  Data = parse_data("../priv/day9input.txt"),
  decompress2(Data).

decompress(Data) ->
  clear(Data, []).

clear([$( | Rest], Acc) ->
  start_marker(Rest, [], Acc);
clear([Char | Rest], Acc) ->
  clear(Rest, [Char | Acc]);
clear([], Acc) ->
  lists:reverse(Acc).

start_marker([$x | Rest], Acc, Full) ->
  mid_marker(Rest, list_to_integer(Acc), [], Full);
start_marker([Char | Rest], Acc, Full) ->
  start_marker(Rest, Acc ++ [Char], Full).

mid_marker([$) | Rest], Positions, Acc, Full) ->
  decompressing(Rest, Positions, list_to_integer(Acc), [], Full);
mid_marker([Char | Rest], Positions, Acc, Full) ->
  mid_marker(Rest, Positions, Acc ++ [Char], Full).

decompressing([Char | Rest], Positions, Repetitions, Sequence, Full) when Positions > 0 ->
  decompressing(Rest, Positions - 1, Repetitions, [Char | Sequence], Full);
decompressing(String, 0, Repetitions, Sequence, Full) when Repetitions > 0 ->
  decompressing(String, 0, Repetitions - 1, Sequence, Sequence ++ Full);
decompressing(String, 0, 0, _Sequence, Full) ->
  clear(String, Full).

decompress2(Data) ->
  MainWorker = spawn(?MODULE, init_worker, [Data, self(), [], 0]),
  receive
    {sum, MainWorker, Sum} ->
      Sum
  end.
%%init_worker(Data, self(), [], 0).

init_worker(Data, ParentPid, ChildPids, Sum) ->
  clear2(Data, ParentPid, ChildPids, Sum).

clear2([$( | Rest], ParentPid, ChildPids, Acc) ->
  start_marker2(Rest, ParentPid, ChildPids, [], Acc);
clear2([_Char | Rest], ParentPid, ChildPids, Acc) ->
  clear2(Rest, ParentPid, ChildPids, Acc + 1);
clear2([], ParentPid, [], Acc) ->
  ParentPid ! {sum, self(), Acc};
%%  Acc;
clear2([], ParentPid, ChildPids, Acc) ->
  receive
    {sum, Pid, SubSum} ->
      {Pid, Multiplicator} = lists:keyfind(Pid, 1, ChildPids),
      clear2([], ParentPid, lists:keydelete(Pid, 1, ChildPids), Acc + Multiplicator * SubSum)
  end.

start_marker2([$x | Rest], ParentPid, ChildPids, Length, Acc) ->
  mid_marker2(Rest, ParentPid, ChildPids, list_to_integer(Length), [], Acc);
start_marker2([Char | Rest], ParentPid, ChildPids, Length, Acc) ->
  start_marker2(Rest, ParentPid, ChildPids, Length ++ [Char], Acc).

mid_marker2([$) | Rest], ParentPid, ChildPids, Length, Repetitions, Acc) ->
  sequence2(Rest, ParentPid, ChildPids, Length, list_to_integer(Repetitions), [], Acc);
mid_marker2([Char | Rest], ParentPid, ChildPids, Length, Repetitions, Acc) ->
  mid_marker2(Rest, ParentPid, ChildPids, Length, Repetitions ++ [Char], Acc).

sequence2([Char | Rest], ParentPid, ChildPids, Length, Repetitions, Sequence, Acc) when length(Sequence) < Length ->
  sequence2(Rest, ParentPid, ChildPids, Length, Repetitions, [Char | Sequence], Acc);
sequence2(FullString, ParentPid, ChildPids, _Length, Repetitions, Sequence, Acc) ->
  NewChild = spawn(?MODULE, init_worker, [lists:reverse(Sequence), self(), [], 0]),
  clear2(FullString, ParentPid, [{NewChild, Repetitions} | ChildPids], Acc).
%%  SubSum = Repetitions * clear2(lists:reverse(Sequence), self(), [], 0),
%%  clear2(FullString, ParentPid, ChildPids, Acc + SubSum).