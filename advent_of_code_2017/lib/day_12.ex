defmodule AdventOfCode2017.Day12 do
  @moduledoc """
  Day 12 solutions
  """

  @doc """
  Solves the first riddle of day 12.
  Examples:

    iex> AdventOfCode2017.Day12.solve("0 <-> 2
    ...> 1 <-> 1
    ...> 2 <-> 0, 3, 4
    ...> 3 <-> 2, 4
    ...> 4 <-> 2, 3, 6
    ...> 5 <-> 6
    ...> 6 <-> 4, 5")
    6

  """
  def solve(input) do
    input
    |> parse
    |> expand_path
    |> length
  end

  @doc """
  Solves the second riddle of day 12. 
  Examples:

    iex> AdventOfCode2017.Day12.solve2("0 <-> 2
    ...> 1 <-> 1
    ...> 2 <-> 0, 3, 4
    ...> 3 <-> 2, 4
    ...> 4 <-> 2, 3, 6
    ...> 5 <-> 6
    ...> 6 <-> 4, 5")
    2
  """
  def solve2(input) do
    input
    |> parse
    |> find_groups(0)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # ------------------------------------------------------------------
  def parse(input) do
    input
    |> String.split("\n")
    |> Enum.map(&(String.split(&1, " <-> ")))
    |> Enum.flat_map(fn [from, tos] -> for to <- String.split(tos, ", "), do: {String.to_integer(to), String.trim(from) |> String.to_integer} end)
    |> Enum.group_by(&(elem(&1,0)), &(elem(&1, 1)))
  end

  defp expand_path(allPaths), do: expand_path(allPaths, [0], allPaths[0])
  defp expand_path(allPaths, explored, [point | toExplore]) do
    case (point in explored) do
      true -> expand_path(allPaths, explored, toExplore)
      false -> expand_path(allPaths, [point | explored], allPaths[point] ++ toExplore )
    end
  end
  defp expand_path(_, explored, []) do
    explored
  end

  defp delete_keys(allPaths, [point | explored]), do: delete_keys(Map.delete(allPaths, point), explored)
  defp delete_keys(allPaths, []), do: allPaths

  defp find_groups(allPaths, groupCount) when allPaths == %{}, do: groupCount
  defp find_groups(allPaths, groupCount) do
    firstKey = Map.keys(allPaths) |> hd
    group = expand_path(allPaths, [firstKey], allPaths[firstKey])
    find_groups(delete_keys(allPaths, group), groupCount + 1)
  end
  
end
