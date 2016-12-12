-module(day12).

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
                  case R of
                    [Char] when Char >= $a,
                                Char =< $d ->
                      {jnz, list_to_atom([Char]), list_to_integer(Offset)};
                    Int ->
                      {jnz, list_to_integer(Int), list_to_integer(Offset)}
                  end
              end,
      [Entry | Acc]
    end, [read], [])).

solve() ->
  solve(parse_data("../priv/day12input.txt"), 0).


solve2() ->
  solve(parse_data("../priv/day12input.txt"), 1).

solve(Instructions, InitCVal) ->
  register(a, spawn(?MODULE, register_proc, [0])),
  register(b, spawn(?MODULE, register_proc, [0])),
  register(c, spawn(?MODULE, register_proc, [InitCVal])),
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
      Reg ! inc,
      parse_program(Addr + 1);
    {cpy, RegFrom, RegTo} when is_atom(RegFrom) ->
      RegFrom ! {get, self()},
      receive
        Value ->
          RegTo ! {cpy, Value},
          parse_program(Addr + 1)
      end;
    {cpy, Value, Reg} ->
      Reg ! {cpy, Value},
      parse_program(Addr + 1);
    {jnz, Reg, Offset} when is_atom(Reg) ->
      Reg ! {zero, self()},
      receive
        false -> parse_program(Addr + Offset);
        true -> parse_program(Addr + 1)
      end;
    {jnz, 0, _Offset} ->
      parse_program(Addr + 1);
    {jnz, _Otherwise, Offset} ->
      parse_program(Addr + Offset)
  end.

memory_proc(Program) ->
  receive
    die -> ok;
    {get, Addr, Pid} ->
      case lists:keyfind(Addr, 1, Program) of
        {_, Instr} ->
%%          io:format("~p: ~p~n", [Addr, Instr]),
          Pid ! Instr,
          memory_proc(Program);
        false ->
          Pid ! false,
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
%%      io:format("Checking for 0 - ~p ~n", [State]),
      Pid ! State =:= 0,
      register_proc(State);
    {get, Pid} ->
      Pid ! State,
      register_proc(State)
  end.

