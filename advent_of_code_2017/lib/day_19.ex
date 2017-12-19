defmodule AdventOfCode2017.Day19 do
  @moduledoc """
  Day 19 solutions
  """

  @doc """
  Solves the first riddle of day 19.
  Examples:

      iex> AdventOfCode2017.Day19.solve("    |          
      ...>    |  +--+    
      ...>    A  |  C    
      ...>F---|----E|--+ 
      ...>    |  |  |  D 
      ...>    +B-+  +--+ ")
      "ABCDEF"
  """
  def solve(input) do
    input
    |> build_map
    |> start_journey
    |> elem(0)
    |> Enum.join
  end

  @doc """
  Solves the second riddle of day 19. 
  Examples:

      iex> AdventOfCode2017.Day19.solve2("    |          
      ...>    |  +--+    
      ...>    A  |  C    
      ...>F---|----E|--+ 
      ...>    |  |  |  D 
      ...>    +B-+  +--+ ")
      38

  """
  def solve2(input) do
    input
    |> build_map
    |> start_journey
    |> elem(1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp build_map(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.codepoints/1)
    |> Enum.map(&Enum.with_index/1)
    |> Enum.map(&(Enum.into(&1, %{}, fn {v,k} -> {k,v} end)))
    |> Enum.with_index
    |> Enum.into(%{}, fn {v,k} -> {k,v} end)
  end

  defp start_journey(map) do
    {startX, _} = Enum.find(Map.to_list(map[0]), fn {_, el} -> el == "|" end)
    go(map, startX, 0, :down, [], 0)
  end

  defp go(map, x, y, dir, letters, steps) when (dir == :down) or (dir == :up) do
    steps = steps + 1
    case map[y][x] do
      nil -> {letters, steps - 1}
      " " -> {letters, steps - 1}
      "|" -> go(map, x, move(dir, y), dir, letters, steps)
      "-" -> go(map, x, move(dir, y), dir, letters, steps)
      "+" -> turn(map, x, y, dir, letters, steps)
      letter -> go(map, x, move(dir, y), dir, letters ++ [letter], steps)
    end
  end
  defp go(map, x, y, dir, letters, steps) when (dir == :left) or (dir == :right) do
    steps = steps + 1
    case map[y][x] do
      nil -> {letters, steps - 1}
      " " -> {letters, steps - 1}
      "|" -> go(map, move(dir, x), y, dir, letters, steps)
      "-" -> go(map, move(dir, x), y, dir, letters, steps)
      "+" -> turn(map, x, y, dir, letters, steps)
      letter -> go(map, move(dir, x), y, dir, letters ++ [letter], steps)
    end
  end

  defp turn(map, x, y, dir, letters, steps) when (dir == :down) or (dir == :up) do
    case map[y][x + 1] do
      " " -> go(map, x - 1, y, :left, letters, steps)
      nil -> go(map, x - 1, y, :left, letters, steps)
      _ -> go(map, x + 1, y, :right, letters, steps)
    end
  end
  defp turn(map, x, y, dir, letters, steps) when (dir == :left) or (dir == :right) do
    case map[y + 1][x] do
      " " -> go(map, x, y - 1, :up, letters, steps)
      nil -> go(map, x, y - 1, :up, letters, steps)
      _ -> go(map, x, y + 1, :down, letters, steps)
    end    
  end

  defp move(:down, y), do: y + 1
  defp move(:up, y), do: y - 1
  defp move(:right, x), do: x + 1
  defp move(:left, x), do: x - 1
end
