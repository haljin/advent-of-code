-module(day23).

-compile(export_all).


parse_data(File) ->
  lists:reverse(santa_utils:map_file(File,
    fun(Line, Acc) ->
      Entry = case Line -- "+\n" of
                "inc " ++ R -> {inc, list_to_atom(R)};
                "dec " ++ R -> {dec, list_to_atom(R)};
                "cpy " ++ Args ->
                  [Val, R] = string:tokens(Args, " "),
                  case Val of
                    [Char] when Char >= $a,
                                Char =< $d ->
                      {cpy, list_to_atom([Char]), list_to_atom(R)};
                    Int ->
                      {cpy, list_to_integer(Int), list_to_atom(R)}
                  end;
                "jnz " ++ Args ->
                  [R, Offset] = string:tokens(Args, " "),
                  ParsedOffset = case Offset of
                                   [Char1] when Char1 >= $a,
                                                Char1 =< $d -> list_to_atom([Char1]);
                                   Int1 -> list_to_integer(Int1)
                                 end,
                  case R of
                    [Char] when Char >= $a,
                                Char =< $d ->
                      {jnz, list_to_atom([Char]), ParsedOffset};
                    Int ->
                      {jnz, list_to_integer(Int), ParsedOffset}
                  end;
                "tgl " ++ Args ->
                  {tgl, list_to_atom(Args)}
              end,
      [Entry | Acc]
    end, [read], [])).

solve() ->
  solve(parse_data("../priv/day23input.txt"), 7).

solve2() ->
  solve(parse_data("../priv/day23input.txt"), 12).

solve(Instructions, InitAVal) ->
  register(a, spawn(?MODULE, register_proc, [InitAVal])),
  register(b, spawn(?MODULE, register_proc, [0])),
  register(c, spawn(?MODULE, register_proc, [0])),
  register(d, spawn(?MODULE, register_proc, [0])),
  Program = lists:zip(lists:seq(1, length(Instructions)), Instructions),
  register(memory, spawn(?MODULE, memory_proc, [Program])),
  parse_program(1),
  a ! {get, self()},
  receive
    State ->
      a ! die,
      b ! die,
      c ! die,
      d ! die,
      memory ! die,
      State
  end.


parse_program(Addr) ->
  memory ! {get, Addr, self()},
  receive
    false -> done;
    {dec, Reg} ->
      Reg ! dec,
      parse_program(Addr + 1);
    {inc, Reg} ->
      {Instr, NextOffset} = maybe_optimize(Addr),
      Reg ! Instr,
      parse_program(Addr + NextOffset);
    {cpy, RegFrom, RegTo} when is_atom(RegFrom) ->
      Value = get_reg(RegFrom),
      RegTo ! {cpy, Value},
      parse_program(Addr + 1);
    {cpy, Value, Reg} when is_atom(Reg) ->
      Reg ! {cpy, Value},
      parse_program(Addr + 1);
    {jnz, Reg, Offset} when is_atom(Reg),
                            is_integer(Offset) ->
      Reg ! {zero, self()},
      receive
        false -> parse_program(Addr + Offset);
        true -> parse_program(Addr + 1)
      end;
    {jnz, Reg, Offset} when is_atom(Reg),
                            is_atom(Offset) ->
      Reg ! {zero, self()},
      receive
        false ->
          Val = get_reg(Offset),
          parse_program(Addr + Val);

        true -> parse_program(Addr + 1)
      end;
    {jnz, 0, _Offset} ->
      parse_program(Addr + 1);
    {jnz, _Otherwise, Offset} when is_integer(Offset) ->
      parse_program(Addr + Offset);
    {jnz, _Otherwise, Offset} when is_atom(Offset) ->
      Val = get_reg(Offset),
      parse_program(Addr + Val);
    {tgl, Reg} ->
      Value = get_reg(Reg),
      memory ! {tgl, Addr + Value},
      parse_program(Addr + 1);
    _ ->
      parse_program(Addr + 1)
  end.

maybe_optimize(Addr) ->
  memory ! {get_sub, Addr, 5,self()},
  receive
    {sub, Sub} ->
      optimize(Sub)
  end.

optimize([{_, {inc, X}},
          {_, {dec, C1}},
          {_, {jnz, C1, -2}},
          {_, {dec, C2}},
          {_, {jnz, C2, -5}}] = Sub) ->
  Val1 = get_reg(C1),
  Val2 = get_reg(C2),
  Init = get_reg(X),
  {{cpy, Init + Val1*Val2}, 5};
optimize([{_, {inc, _X}}|_]) ->
  {inc, 1}.

get_reg(Reg) ->
  Reg ! {get, self()},
  receive
    Val -> Val
  end.

memory_proc(Program) ->
  receive
    die -> ok;
    {get, Addr, Pid} ->
      case lists:keyfind(Addr, 1, Program) of
        {_, Instr} ->
          Pid ! Instr,
          memory_proc(Program);
        false ->
          Pid ! false,
          memory_proc(Program)
      end;
    {get_sub, Addr, Length, Pid} ->
      Sub = lists:sublist(Program, Addr, Length),
      Pid ! {sub, Sub},
      memory_proc(Program);
    {tgl, Addr} ->
      case lists:keyfind(Addr, 1, Program) of
        {Addr, {inc, Arg}} ->
          memory_proc(lists:keyreplace(Addr, 1, Program, {Addr, {dec, Arg}}));
        {Addr, {_, Arg}} ->
          memory_proc(lists:keyreplace(Addr, 1, Program, {Addr, {inc, Arg}}));
        {Addr, {jnz, Arg1, Arg2}} ->
          memory_proc(lists:keyreplace(Addr, 1, Program, {Addr, {cpy, Arg1, Arg2}}));
        {Addr, {_, Arg1, Arg2}} ->
          memory_proc(lists:keyreplace(Addr, 1, Program, {Addr, {jnz, Arg1, Arg2}}));
        _ ->
          memory_proc(Program)
      end
  end.

register_proc(State) ->
  receive
    die -> ok;
    dec -> register_proc(State - 1);
    inc -> register_proc(State + 1);
    {cpy, Val} ->
      register_proc(Val);
    {zero, Pid} ->
      Pid ! State =:= 0,
      register_proc(State);
    {get, Pid} ->
      Pid ! State,
      register_proc(State)
  end.

