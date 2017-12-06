defmodule AdventOfCode2017.Day6 do
  @moduledoc """
  Day 6 solutions
  """

  @doc """
  Solves the first riddle of day 6.
  Examples:

      iex> AdventOfCode2017.Day6.solve("0 2 7 0")
      5

  """
  def solve(input) do
    input
    |> String.split
    |> Enum.map(&String.to_integer/1)
    |> do_solve
    |> elem(0)
  end

  @doc """
  Solves the second riddle of day 6. 
  Examples:

      iex> AdventOfCode2017.Day6.solve2("0 2 7 0")
      4

  """
  def solve2(input) do
    input
    |> String.split
    |> Enum.map(&String.to_integer/1)
    |> do_solve
    |> elem(1)
    |> do_solve
    |> elem(0)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp do_solve(memory) do
    redistribute(0, memory, [])
  end

  defp redistribute(step, memory, seen) do
    checksum = :erlang.list_to_binary(memory)
    case Enum.member?(seen, checksum) do
      true -> {step, memory}
      false ->       
        max = Enum.max(memory)
        {before, [max | rest]} = Enum.split_while(memory, &(&1 != max))
        redistribute_blocks(step, before ++ [0], rest, max, [checksum | seen])
    end
  end

  defp redistribute_blocks(step, visited, not_visited, 0, seen) do
    redistribute(step + 1, visited ++ not_visited, seen)
  end
  defp redistribute_blocks(step, visited, [val | rest], blocks, seen) do
    redistribute_blocks(step, visited ++ [val + 1], rest, blocks - 1, seen)
  end
  defp redistribute_blocks(step, visited, [], blocks, seen) do
    redistribute_blocks(step, [], visited, blocks, seen)
  end
end
