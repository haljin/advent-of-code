%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  @doc
%%  Each logic gate is a process. The gate registers itself in the wires ETS
%%  on each of the wires its inputs are connected to. Once the logic gate
%%  receives all of its inputs it looks the ETS table in order to find all the
%%  gates connected to its output wire and sends them its state.
%%
%%  In that way ETS table acts as the wires network - those could also be
%%  spawned as their own processes instead.
%%
%%  In order for this to work, the actual inputs (the wires which have known
%%  value from start) are inputted last.
%%
%%  @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(day7).

-compile(export_all).


parse_data(Path) ->
  santa_utils:map_file(Path, fun(X, Acc) ->
    Entry = string:tokens(string:strip(X, both, $\n), " "),
    Acc ++ [Entry]
                              end, [read], []).

override_for_2(Data, Override) ->
  [Match] = [M || [_,_,"b"] = M <- Data],
  (Data -- [Match]) ++ [[integer_to_list(Override), "->", "b"]].


solve(ListofStrings) ->
  ets:new(wires, [named_table, bag]),
  process_day7(ListofStrings, []).

process_day7([["NOT", StrInput, "->",  StrOutput]| T], Inputs) ->
  Input = list_to_atom(StrInput), Output = list_to_atom(StrOutput),
  Pid = spawn(?MODULE, gate1, [fun(In1) -> <<Out:16>> = <<(bnot In1):16>>, Out end, Output]),
  ets:insert(wires, {Pid, Input}),
  process_day7(T, Inputs);
process_day7([[StrInput1, "OR", StrInput2, "->", StrOutput]| T], Inputs) ->
  Output = list_to_atom(StrOutput),
  Pid = spawn(?MODULE, gate2, [fun(In1, In2) -> <<Out:16>> = <<(In1 bor In2):16>>, Out end, Output]),
  process_input(StrInput1, Pid),
  process_input(StrInput2, Pid),
  process_day7(T, Inputs);
process_day7([[StrInput1, "AND", StrInput2, "->", StrOutput]| T], Inputs) ->
  Output = list_to_atom(StrOutput),
  Pid = spawn(?MODULE, gate2, [fun(In1, In2) -> <<Out:16>> = <<(In1 band In2):16>>, Out end, Output]),
  process_input(StrInput1, Pid),
  process_input(StrInput2, Pid),
  process_day7(T, Inputs);
process_day7([[StrInput, "RSHIFT", StrOffset, "->", StrOutput]| T], Inputs) ->
  Input = list_to_atom(StrInput), Offset = list_to_integer(StrOffset), Output = list_to_atom(StrOutput),
  Pid = spawn(?MODULE, gate1, [fun(In1) -> <<Out:16>> = <<(In1 bsr Offset):16>>, Out end, Output]),
  ets:insert(wires, {Pid, Input}),
  process_day7(T, Inputs);
process_day7([[StrInput, "LSHIFT", StrOffset, "->", StrOutput]| T], Inputs) ->
  Input = list_to_atom(StrInput), Offset = list_to_integer(StrOffset), Output = list_to_atom(StrOutput),
  Pid = spawn(?MODULE, gate1, [fun(In1) -> <<Out:16>> = <<(In1 bsl Offset):16>>, Out end, Output]),
  ets:insert(wires, {Pid, Input}),
  process_day7(T, Inputs);
process_day7([[StrInput, "->", StrOutput]| T], Inputs) ->
  Output = list_to_atom(StrOutput),
  case string:to_integer(StrInput) of
    {error, no_integer} ->
      Input = list_to_atom(StrInput),
      Pid = spawn(?MODULE, gate1, [fun(In1) -> In1 end, Output]),
      ets:insert(wires, {Pid, Input}),
      process_day7(T, Inputs);
    {Input, _} ->
      process_day7(T, [{Input, Output}| Inputs])
  end;
process_day7([], Inputs) ->
  [begin
     Pids = ets:select(wires, [{{'$1', Output}, [], ['$1']}]),
     io:format("Pure Input ~p: ~p~n", [Output, Input]),
     [Pid ! {input, Input} || Pid <- Pids]
   end || {Input, Output} <- Inputs].

process_input(StrInput, Pid) ->
  case string:to_integer(StrInput) of
    {error, no_integer} ->
      Input = list_to_atom(StrInput),
      ets:insert(wires, {Pid, Input});
    {Input, _} ->
      Pid ! {input, Input}
  end.

gate1(Fun, Output) ->
  receive
    {input, In} ->
      io:format("Gate got input ~p, sending to ~p~n", [In, Output]),
      Pids = ets:select(wires, [{{'$1', Output}, [], ['$1']}]),
      OutVal = Fun(In),
      io:format("~p: ~p~n", [Output, OutVal]),
      %ets:insert(signals, {Output, OutVal}),
      [Pid ! {input, OutVal} || Pid <- Pids]
  end.


gate2(Fun, Output) ->
  receive
    {input, In} ->
      io:format("Gate got input ~p, to ~p, waiting for another ~n", [In, Output]),
      gate2(Fun, In, Output)
  end.

gate2(Fun, In2, Output) ->
  receive
    {input, In} ->
      io:format("Gate got input ~p and ~p, sending to ~p~n", [In2, In, Output]),
      Pids = ets:select(wires, [{{'$1', Output}, [], ['$1']}]),
      OutVal = Fun(In2, In),
      io:format("~p: ~p~n", [Output, OutVal]),
      %ets:insert(signals, {Output, OutVal}),
      [Pid ! {input, OutVal} || Pid <- Pids]
  end.