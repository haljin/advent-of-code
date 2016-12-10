-module(day10).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(     File, fun(X, Acc) ->
    Acc ++ [X -- "\n"] end, [read], []).

solve() ->
  Data = parse_data("../priv/day10input.txt"),
  {Result, _} = solve(Data, []),
  Result.

solve2() ->
  Data = parse_data("../priv/day10input.txt"),
  {_, Result} = solve(Data, []),
  lists:foldl(fun(E, Acc) -> E * Acc end, 1, Result).

solve(["bot " ++ _Rest = Instruction | Rest], Pids) ->
  [Entity1, No1, _, _, _, Entity2, No2, _, _, _, Entity3, No3] = string:tokens(Instruction, " "),
  {NewPids, [Bot1, Bot2, Bot3]} = maybe_spawn([{Entity1, No1}, {Entity2, No2}, {Entity3, No3}], Pids, []),
  Bot1 ! {give, Bot2, Bot3},
  solve(Rest, NewPids);
solve(["value " ++ _Rest = Instruction | Rest], Pids) ->
  [_, ValueString, _, _, Entity, No] = string:tokens(Instruction, " "),
  {NewPids, [Bot1]} = maybe_spawn([{Entity, No}], Pids, []),
  Bot1 ! {get_token, list_to_integer(ValueString)},
  solve(Rest, NewPids);
solve([], Pids) ->
  Outputs = [OutId || {{"output", OutId}, _Pid} <- Pids],
  Results = collect_results([0, 1, 2], length(Outputs), undefined, []),
  [Pid ! die || {_, Pid} <- Pids],
  Results.

collect_results(_Ids, 0, Result, Acc) when Result =/= undefined ->
  {Result, Acc};
collect_results(Ids, AllOutputs, OldResult, Acc) ->
  receive
    {result, Result} -> collect_results(Ids, AllOutputs, Result, Acc);
    {output, Id, Token} ->
      case lists:member(Id, Ids) of
        true ->
          collect_results(Ids, AllOutputs - 1, OldResult, [Token | Acc]);
        false ->
          collect_results(Ids, AllOutputs - 1, OldResult, Acc)
      end
  end.


bot_proc(MyId, HostId, Tokens) ->
  receive
    {get_token, Token} when length(Tokens) < 2 ->
      bot_proc(MyId, HostId, lists:sort([Token | Tokens]));
    {give, Bot1, Bot2} when length(Tokens) =:= 2 ->
      case lists:sort(Tokens) of
        [17, 61] -> HostId ! {result, MyId};
        _ -> nevermind
      end,
      [Low, High] = Tokens,
      Bot1 ! {get_token, Low},
      Bot2 ! {get_token, High},
      bot_proc(MyId, HostId, []);
    die ->
      ok
  end.

output_proc(MyId, HostId) ->
  receive
    {get_token, Token} ->
      HostId ! {output, MyId, Token},
      output_proc(MyId, HostId);
    die ->
      ok
  end.

maybe_spawn([{Entity, No} | Rest], Pids, Acc) ->
  case lists:keyfind({Entity, No}, 1, Pids) of
    {{Entity, No}, Pid} ->
      maybe_spawn(Rest, Pids, Acc ++ [Pid]);
    false ->
      {EntityFun, EntityArgs} = case Entity of
                                  "bot" -> {bot_proc, [list_to_integer(No), self(), []]};
                                  "output" -> {output_proc, [list_to_integer(No), self()]}
                                end,
      Pid = spawn(?MODULE, EntityFun, EntityArgs),
      maybe_spawn(Rest, [{{Entity, No}, Pid} | Pids], Acc ++ [Pid])
  end;
maybe_spawn([], Pids, Acc) ->
  {Pids, Acc}.

