-module(day23).

-compile(export_all).

parse_data(File) ->
  lists:reverse(santa_utils:map_file(File,
    fun(Line, Acc) ->
      Entry = case Line -- "+\n" of
                "hlf " ++ R -> {hlf, list_to_atom(R)};
                "tpl " ++ R -> {tpl, list_to_atom(R)};
                "inc " ++ R -> {inc, list_to_atom(R)};
                "jmp " ++ Offset -> {jmp, list_to_integer(Offset)};
                "jie " ++ Args ->
                  [R, Offset] = string:tokens(Args, ", "),
                  {jie, list_to_atom(R), list_to_integer(Offset)};
                "jio " ++ Args ->
                  [R, Offset] = string:tokens(Args, ", "),
                  {jio, list_to_atom(R), list_to_integer(Offset)}
              end,
      [Entry | Acc]
    end, [read], [])).

solve(Instructions) ->
  register(a, spawn(?MODULE, register_proc, [0])),
  register(b, spawn(?MODULE, register_proc, [0])),
  Program = lists:zip(lists:seq(1, length(Instructions)), Instructions),
  register(memory, spawn(?MODULE, memory_proc, [Program])),
  parse_program(1),
  b ! {get, self()},
  receive
    State ->
      a ! die,
      b ! die,
      memory ! die,
      State
  end.

solve2(Instructions) ->
  register(a, spawn(?MODULE, register_proc, [1])),
  register(b, spawn(?MODULE, register_proc, [0])),
  Program = lists:zip(lists:seq(1, length(Instructions)), Instructions),
  register(memory, spawn(?MODULE, memory_proc, [Program])),
  parse_program(1),
  b ! {get, self()},
  receive
    State ->
      a ! die,
      b ! die,
      memory ! die,
      State
  end.

parse_program(Addr) ->
  memory ! {get, Addr, self()},
  receive
    false -> done;
    {hlf, Reg} ->
      Reg ! half,
      parse_program(Addr + 1);
    {tpl, Reg} ->
      Reg ! triple,
      parse_program(Addr + 1);
    {inc, Reg} ->
      Reg ! inc,
      parse_program(Addr + 1);
    {jmp, Offset} ->
      parse_program(Addr + Offset);
    {jie, Reg, Offset} ->
      Reg ! {even, self()},
      receive
        true -> parse_program(Addr + Offset);
        false -> parse_program(Addr + 1)
      end;
    {jio, Reg, Offset} ->
      Reg ! {one, self()},
      receive
        true -> parse_program(Addr + Offset);
        false -> parse_program(Addr + 1)
      end
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
          Pid ! false  ,
          memory_proc(Program)
      end
  end.

register_proc(State) ->
  receive
    die -> ok;
    half -> register_proc(State div 2);
    triple -> register_proc(State * 3);
    inc -> register_proc(State + 1);
    {even, Pid} ->
      Pid ! (State rem 2) =:= 0,
      register_proc(State);
    {one, Pid} ->
      Pid ! State =:= 1,
      register_proc(State);
    {get, Pid} ->
      Pid ! State,
      register_proc(State)
  end.





