defmodule AdventOfCode2017.Day2 do
  @moduledoc """
  Day 2 solutions
  """

  @doc """
  Solves the first riddle of day 2. 
  Examples:

      iex> AdventOfCode2017.Day2.solve("
      ...> 5 1 9 5
      ...> 7 5 3
      ...> 2 4 6 8")
      18
  """
  def solve(input) do
    do_solve(input, &(&2 + (Enum.max(&1) - Enum.min(&1))))
  end

  @doc """
  Solves the second riddle of day 2. 
  Examples:

      iex> AdventOfCode2017.Day2.solve2("
      ...> 5 9 2 8
      ...> 9 4 7 3
      ...> 3 8 6 5")
      9
  """
  def solve2(input) do
    do_solve(input, &find_div/2)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp do_solve(input, function) do
    input |>
    String.split("\n") |>
    Enum.filter(&(&1 != "")) |>
    Enum.map(&split_row/1) |>
    Enum.reduce(0, function) 
  end

  defp split_row(row) do
    row |>
    String.trim |>
    String.split |>
    Enum.map(&String.to_integer/1)
  end

  def find_div([element | rest], acc) do
    value = Enum.find(rest, &((rem(&1, element) == 0) or (rem(element, &1) == 0)))
    case value do
      nil -> find_div(rest, acc)
      _ -> acc + div(max(element, value), min(element, value))
    end
  end
end
