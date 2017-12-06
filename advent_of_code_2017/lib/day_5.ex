defmodule AdventOfCode2017.Day5 do
  @moduledoc """
  Day 5 solutions
  """

  @doc """
  Solves the first riddle of day 5.
  Examples:

      iex> AdventOfCode2017.Day5.solve("
      ...> 0
      ...> 3
      ...> 0
      ...> 1
      ...> -3
      ...> ")
      5

  """
  def solve(input) do
    parse_data(input)
    |> move_on_list(1, 0)
  end

  @doc """
  Solves the second riddle of day 5. 
  Examples:

      iex> AdventOfCode2017.Day5.solve2("
      ...> 0
      ...> 3
      ...> 0
      ...> 1
      ...> -3
      ...> ")
      10

  """
  def solve2(input) do
    parse_data(input)
    |> move_on_list2(1, 0)    
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp parse_data(input) do
    input
    |> String.split
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index(1)
    |> Enum.into(%{}, fn {v, k} -> {k, v} end)
  end
  
  defp move_on_list(map, position, step) do
    case map[position] do
      nil -> step
      val -> move_on_list(%{map | position => val + 1}, position + val, step + 1)
    end    
  end

  defp move_on_list2(map, position, step) do
    case map[position] do
      nil -> step
      val when val >= 3 -> move_on_list2(%{map | position => val - 1}, position + val, step + 1)
      val -> move_on_list2(%{map | position => val + 1}, position + val, step + 1)
    end    
  end
  
end
