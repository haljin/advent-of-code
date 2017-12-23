defmodule AdventOfCode2017.Day23 do
  @moduledoc """
  Day 23 solutions
  """
  @init_regs %{a: 0, b: 0, c: 0, d: 0, e: 0, f: 0, h: 0}
  @second_regs %{a: 1, b: 0, c: 0, d: 0, e: 0, f: 0, h: 0}

  @doc """
  Solves the first riddle of day 23.
  """
  def solve(input) do
    input
    |> parse
    |> execute
    |> elem(0)
  end

  @doc """
  Solves the second riddle of day 23. 
  """
  def solve2(input) do
    input
    |> parse
    |> execute(0, @second_regs)
    |> elem(1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp execute(program, adrPtr \\ 0, registers \\ @init_regs, muls \\ 0) do
    case program[adrPtr] do
      nil -> {muls, registers.h}
      {jump, check, offset} when jump == :jnz or jump == :jz->
        result = op(jump, check, offset, registers)
        execute(program, adrPtr + result, registers, muls)
      {instr, reg, arg} ->
        result = op(instr, reg, arg, registers)
        countMuls = if instr == :mul, do: muls + 1, else: muls
        execute(program, adrPtr + 1, Map.put(registers, reg, result), countMuls)
    end  
  end

  defp op(cmd, first, second, registers) when is_atom(first), do: op(cmd, Map.get(registers, first, 0), second, registers)
  defp op(cmd, first, second, registers) when is_atom(second), do: op(cmd, first, Map.get(registers, second, 0), registers)
  defp op(:set, _, second, _registers), do: second
  defp op(:sub, first, second, _registers), do: first - second
  defp op(:mul, first, second, _registers), do: first * second
  defp op(:mod, first, second, _registers), do: rem(first, second)
  defp op(:jnz, first, second, _registers), do: if first != 0, do: second, else: 1
  defp op(:jz, first, second, _registers), do: if first == 0, do: second, else: 1


  defp parse(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&parse_instr/1)
    |> Enum.with_index
    |> Enum.into(%{}, fn {k,v} -> {v, k} end)
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
