defmodule AdventOfCode2017.Day18 do
  @moduledoc """
  Day 18 solutions
  """

  @doc """
  Solves the first riddle of day 18.
  Examples:

      iex> AdventOfCode2017.Day18.solve("set a 1
      ...> add a 2
      ...> mul a a
      ...> mod a 5
      ...> snd a
      ...> set a 0
      ...> rcv a
      ...> jgz a -1
      ...> set a 1
      ...> jgz a -2")
      4
  """
  def solve(input) do
    input
    |> parse
    |> execute    
  end

  @doc """
  Solves the second riddle of day 18. 
  Examples:

      iex> AdventOfCode2017.Day18.solve2("snd 1
      ...> snd 2
      ...> snd p
      ...> rcv a
      ...> rcv b
      ...> rcv c
      ...> rcv d")
      3

  """
  def solve2(input) do
    program = parse(input)
    first = spawn(__MODULE__, :concurrent_init, [program, nil, %{p: 0}, 0, self()])
    _second = spawn(__MODULE__, :concurrent_init, [program, first, %{p: 1}, 1, self()])
    counter_proc(0)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp execute(program, adrPtr \\ 0, registers \\ %{}, sound \\ nil) do
    # IO.puts "Executing #{inspect program[adrPtr]} register: #{inspect registers}"
    case program[adrPtr] do
      nil -> nil
      {:snd, val} when is_integer(val) -> 
        execute(program, adrPtr + 1, registers, val)
      {:snd, val} -> 
        execute(program, adrPtr + 1, registers, Map.get(registers, val, 0))
      {:rcv, reg} -> 
        case Map.get(registers, reg, 0) do
          0 -> execute(program, adrPtr + 1, registers, sound)
          _val -> sound
        end
      {:jgz, check, offset} ->
        execute(program, adrPtr + jumpgz(check, offset, registers), registers, sound)
      {instr, reg, val} when is_integer(val) -> 
        baseVal = Map.get(registers, reg, 0)
        execute(program, adrPtr + 1, Map.put(registers, reg, op(instr, baseVal, val)), sound)
      {instr, reg, otherReg} ->
        baseVal = Map.get(registers, reg, 0)
        val = Map.get(registers, otherReg, 0)
        execute(program, adrPtr + 1, Map.put(registers, reg, op(instr, baseVal, val)), sound)
    end  
  end
  
  def concurrent_init(program, nil, registers, id, counterPid) do
    receive do
      {:neighbour, pid} ->
        concurrent_execute(program, pid, 0, registers, id, counterPid)
    end
  end
  def concurrent_init(program, pid, registers, id, counterPid) do
    send(pid, {:neighbour, self()})
    concurrent_execute(program, pid, 0, registers, id, counterPid)
  end

  defp concurrent_execute(program, otherPid, adrPtr, registers, id, counterPid)do
    # IO.puts "Executing #{inspect program[adrPtr]} register: #{inspect registers}"
    case program[adrPtr] do
      nil -> nil
      {:snd, val} when is_integer(val) -> 
        send(otherPid, {:val, val})
        send(counterPid, {:tick, id})
        concurrent_execute(program, otherPid, adrPtr + 1, registers, id, counterPid)
      {:snd, val} -> 
        send(otherPid, {:val, Map.get(registers, val, 0)})
        send(counterPid, {:tick, id})
        concurrent_execute(program, otherPid, adrPtr + 1, registers, id, counterPid)
      {:rcv, reg} -> 
        receive do
          {:val, val} ->
            concurrent_execute(program, otherPid, adrPtr + 1, Map.put(registers, reg, val), id, counterPid)
        after 1000 ->
          send(counterPid, {:dead, id})
        end
      {:jgz, check, offset} ->
        concurrent_execute(program, otherPid, adrPtr + jumpgz(check, offset, registers), registers, id, counterPid)
      {instr, reg, val} when is_integer(val) -> 
        baseVal = Map.get(registers, reg, 0)
        concurrent_execute(program, otherPid, adrPtr + 1, Map.put(registers, reg, op(instr, baseVal, val)), id, counterPid)
      {instr, reg, otherReg} ->
        baseVal = Map.get(registers, reg, 0)
        val = Map.get(registers, otherReg, 0)
        concurrent_execute(program, otherPid, adrPtr + 1, Map.put(registers, reg, op(instr, baseVal, val)), id, counterPid)
    end  
  end

  def counter_proc(count) do
    receive do
      {:tick, 1} -> counter_proc(count + 1)
      {:tick, _} -> counter_proc(count)
      {:dead, 0} -> count
    end
  end

  defp jumpgz(check, offset, registers) when is_atom(check) do
    jumpgz(Map.get(registers, check, 0), offset, registers)
  end
  defp jumpgz(check, offset, registers) when is_atom(offset) do
    jumpgz(check, Map.get(registers, offset, 0), registers)
  end
  defp jumpgz(check, offset, _registers) do
    if check > 0, do: offset, else: 1
  end

  defp op(:set, _, otherVal), do: otherVal
  defp op(:add, val, otherVal), do: val + otherVal
  defp op(:mul, val, otherVal), do: val * otherVal
  defp op(:mod, val, otherVal), do: rem(val, otherVal)


  defp parse(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&parse_instr/1)
    |> Enum.with_index
    |> Enum.into(%{}, fn {k,v} -> {v, k} end)
  end
  
  defp parse_instr("rcv " <> rest) do
    {:rcv, String.to_atom(rest)}
  end
  defp parse_instr("snd " <> rest) do
    {:snd, String.to_atom(rest)}
  end
  defp parse_instr(cmd) do
    [instr, to, val] = String.split(cmd)
    {parsedTo, parsedVal} = 
    case {Integer.parse(to), Integer.parse(val)} do
      {:error, :error} -> {String.to_atom(to), String.to_atom(val)}
      {:error, {int, _}} -> {String.to_atom(to), int}
      {{int, _}, :error} -> {int, String.to_atom(val)}
      {{int, _}, {otherInt, _}} -> {int, otherInt}
    end
    {String.to_atom(instr), parsedTo, parsedVal}
  end  
end
