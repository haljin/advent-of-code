-module(day22).

-compile(export_all).
-record(partition, {x,
                    y,
                    size,
                    used,
                    available,
                    use_pct}).

parse_data(File) ->
  santa_utils:map_file(File, fun(Line, Acc) ->
    Add = case string:tokens(Line, " -T%\n") of
            ["/dev/grid/node", "x" ++ StringX, "y" ++ StringY, StringSize, StringUsed, StringAvail, StringPct] ->
              [#partition{x         = list_to_integer(StringX),
                          y         = list_to_integer(StringY),
                          size      = list_to_integer(StringSize),
                          used      = list_to_integer(StringUsed),
                          available = list_to_integer(StringAvail),
                          use_pct   = list_to_integer(StringPct)}];
            _ ->
              []

          end,
    Acc ++ Add end,    [read], []).


solve() ->
  Data = parse_data("../priv/day22input.txt"),
  Pairs = find_pairs(Data, Data, []),
  length(Pairs).

find_pairs([Node | OtherNodes], AllNodes, Acc) ->
  find_pairs(OtherNodes, AllNodes, Acc ++ nodes_with_space(Node, AllNodes, []));
find_pairs([], _, Acc) ->
  Acc.

nodes_with_space(#partition{x = X, y = Y} = Node, [#partition{x = X, y = Y} | Rest], Acc) ->
  nodes_with_space(Node, Rest, Acc);
nodes_with_space(#partition{used = Size} = Node, [#partition{available = Available} = OtherNode | Rest], Acc) when Size =< Available,
                                                                                                                   Size > 0 ->
  nodes_with_space(Node, Rest, [{Node, OtherNode} | Acc]);
nodes_with_space(Node, [_ | Rest], Acc) ->
  nodes_with_space(Node, Rest, Acc);
nodes_with_space(_Node, [], Acc) ->
  Acc.
