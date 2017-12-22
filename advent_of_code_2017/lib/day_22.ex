defmodule AdventOfCode2017.Day22 do
  @moduledoc """
  Day 22 solutions
  """

  @doc """
  Solves the first riddle of day 22.
  Examples:

      iex> AdventOfCode2017.Day22.solve("..#
      ...> #..
      ...> ...", 7)
      5

      iex> AdventOfCode2017.Day22.solve("..#
      ...> #..
      ...> ...", 70)
      41

      iex> AdventOfCode2017.Day22.solve("..#
      ...> #..
      ...> ...")
      5587
  """
  def solve(input, steps \\ 10000) do
    input
    |> parse
    |> burst(steps, &change_simple/1, &count_simple_infections/2)
  end

  @doc """
  Solves the second riddle of day 22. 
  Examples:

      iex> AdventOfCode2017.Day22.solve2("..#
      ...> #..
      ...> ...", 100)
      26

      iex> AdventOfCode2017.Day22.solve2("..#
      ...> #..
      ...> ...")
      2511944
  """
  def solve2(input, steps \\ 10000000) do
    input
    |> parse
    |> burst(steps, &change_advanced/1, &count_adv_infections/2)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp parse(input) do
    String.split(input, "\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.codepoints/1)
    |> Enum.reverse
    |> Enum.map(&(Enum.with_index(&1, -div(length(&1), 2))))
    |> (fn rows -> Enum.with_index(rows, -div(length(rows), 2)) end).()
    |> Enum.reduce(%{}, fn ({row, y}, map) ->
                          Enum.reduce(row, map,
                            fn({el, x}, map) ->
                              Map.put(map, {x, y}, el)
                            end)
                        end)
  end

  defp burst(map, n, change_fun, count_fun, coords \\ {0, 0}, direction \\ :up, infections \\ 0)
  defp burst(_map, 0, _, _, _coords, _direction, infections), do: infections
  defp burst(map, n, change_fun, count_fun, coords, direction, infections) do
    case Map.get(map, coords, ".") do
      el -> 
        newMap = Map.put(map, coords, change_fun.(el))
        newDirection = turn(direction, el)
        burst(newMap, n - 1, change_fun, count_fun, move(newDirection, coords), newDirection, count_fun.(el, infections))
    end
  end

  defp change_simple("."), do: "#"
  defp change_simple("#"), do: "."
  
  defp change_advanced("."), do: "W"
  defp change_advanced("W"), do: "#"
  defp change_advanced("#"), do: "F"
  defp change_advanced("F"), do: "."

  defp count_simple_infections(".", infections), do: infections + 1 
  defp count_simple_infections("#", infections), do: infections

  defp count_adv_infections("W", infections), do: infections + 1
  defp count_adv_infections(_, infections), do: infections

  defp turn(:up, "."), do: :left
  defp turn(:left, "."), do: :down
  defp turn(:down, "."), do: :right
  defp turn(:right, "."), do: :up
  defp turn(:up, "#"), do: :right
  defp turn(:left, "#"), do: :up
  defp turn(:down, "#"), do: :left
  defp turn(:right, "#"), do: :down
  defp turn(:up, "F"), do: :down
  defp turn(:left, "F"), do: :right
  defp turn(:down, "F"), do: :up
  defp turn(:right, "F"), do: :left
  defp turn(dir, "W"), do: dir

  defp move(:up, {x, y}), do: {x, y + 1}
  defp move(:down, {x, y}), do: {x, y - 1}
  defp move(:left, {x, y}), do: {x - 1, y}
  defp move(:right, {x, y}), do: {x + 1, y}
end
