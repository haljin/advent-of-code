defmodule AdventOfCode2017.Day17 do
  @moduledoc """
  Day 17 solutions
  """

  @doc """
  Solves the first riddle of day 17.
  Examples:

      iex> AdventOfCode2017.Day17.solve(3)
      638

  """
  def solve(input) do
    input
    |> insert_val(1, [0], 0, 2017)
    |> find_val_after(2017)
  end

  @doc """
  Solves the second riddle of day 17. 

  """
  def solve2(input) do
    input
    |> smart_insert(1, 0, 0, 50000000)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp insert_val(step, n, list, startPos, lastVal) when n <= lastVal do
    index = rem(step + startPos, n) + 1
    newList = List.insert_at(list, index, n)
    insert_val(step, n + 1, newList, index, lastVal)
  end
  defp insert_val(_, _, list, _, _), do: list

  defp smart_insert(_step, lastVal, valAfter, _, lastVal), do: valAfter
  defp smart_insert(step, n, valAfter, startPos, lastVal) do
    case rem(step + startPos, n) + 1 do
      1 -> smart_insert(step, n + 1, n, 1, lastVal)
      index -> smart_insert(step, n + 1, valAfter, index, lastVal)
    end
  end

  defp find_val_after(list, val) do
    index = Enum.find_index(list, &(&1 == val))
    Enum.at(list, index + 1)
  end
end
