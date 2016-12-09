-module(day9).

-compile(export_all).

-record(marker, {length,
                 reptitions,
                 sub_markers}).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, Line} = file:read_line(Fd),
  Line -- "\n".

solve() ->
  Data = parse_data("../priv/day9input.txt"),
  Decompressed = decompress(Data),
  length(Decompressed).

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
