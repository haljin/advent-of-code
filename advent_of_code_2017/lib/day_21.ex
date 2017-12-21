defmodule AdventOfCode2017.Day21 do
  @moduledoc """
  Day 21 solutions
  """

  @doc """
  Solves the first riddle of day 21.
  Examples:

      iex> AdventOfCode2017.Day21.solve("../.# => ##./#../...
      ...> .#./..#/### => #..#/..../..../#..#", 2)
      12
  """
  def solve(input, iterations \\ 5) do
    default_map = create_pattern(".#./..#/###")
    input
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&(String.split(&1, " => ")))
    |> Enum.map(fn [from, to] -> {create_pattern(from) |> permute_pattern, create_pattern(to)} end)
    |> iterate(default_map, iterations)
    |> Map.values
    |> Enum.flat_map(&Map.values/1)
    |> Enum.filter(&(&1 == "#"))
    |> Enum.count
  end

  @doc """
  Solves the second riddle of day 21. 
  """
  def solve2(input) do
    solve(input, 18)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp create_pattern(string) do
    String.split(string, "/")
    |> Enum.map(&String.codepoints/1)
    |> Enum.map(&Enum.with_index/1)
    |> Enum.map(&(Enum.into(&1,%{}, fn {v, k} -> {k,v} end)))
    |> Enum.with_index
    |> Enum.into(%{}, fn {v, k} -> {k,v} end)
  end
  
  defp permute_pattern(pattern) do
    flippedX = flip_map_x(pattern)
    flippedY = flip_map_y(pattern)
    [
      pattern,rotate_map(pattern), rotate_map(rotate_map(pattern)), rotate_map(rotate_map(rotate_map(pattern))),
      flippedX,rotate_map(flippedX), rotate_map(rotate_map(flippedX)), rotate_map(rotate_map(rotate_map(flippedX))),
      flippedY,rotate_map(flippedY), rotate_map(rotate_map(flippedY)), rotate_map(rotate_map(rotate_map(flippedY)))
    ] 
    |> Enum.uniq
  end

  defp iterate(_rules, map, 0), do: map
  defp iterate(rules, map, n) do
    chunk_map(map)
    |> apply_rules(rules, [[]])
    |> dechunk_map
    |> (fn nextMap -> iterate(rules, nextMap, n - 1) end).()
  end
  
  defp chunk_map(map) do
    divider = if rem(map_size(map), 2) == 0, do: 2, else: 3
    for y <- 0..(div(map_size(map), divider) - 1) do
      for x <- 0..(div(map_size(map), divider) - 1) do
        for newY <- 0..(divider - 1), into: %{} do
          row = for newX <- 0..(divider - 1), into: %{}, do: {newX, map[y * divider + newY][x * divider + newX]}
          {newY, row}
        end
      end
    end
  end

  defp apply_rules([[]], _rules, transformed), do: Enum.reverse(transformed)
  defp apply_rules([[] | rest], rules, transformed) do
    apply_rules(rest, rules, [[] | transformed])
  end
  defp apply_rules([[pattern | rowRest] | columnRest], rules, [ transformedRow | restTransformed]) do
    {_, transformed} = Enum.find(rules, fn {from, _to} -> pattern in from end)
    apply_rules([rowRest | columnRest], rules, [transformedRow ++ [transformed] | restTransformed])
  end

  defp dechunk_map([[someChunk | _] | _] = chunks) do
    dechunk_map(chunks, 0, 0, build_empty_map(map_size(someChunk) * length(chunks)))
  end
  defp dechunk_map([[]], _, _, map), do: map
  defp dechunk_map([[] | rest], _x, y, map), do: dechunk_map(rest, 0, y + 1, map)
  defp dechunk_map([[chunk | restRow] | rest], x, y, map) do
    chunkSize = map_size(chunk)
    newMap = 
    Enum.reduce(0..(chunkSize - 1), map, 
      fn (yIndex, partialMap) ->
        Enum.reduce(0..(chunkSize - 1), partialMap,
          fn (xIndex, morePartialMap) ->
            realMapX = x * chunkSize + xIndex
            realMapY = y * chunkSize + yIndex
            Map.put(morePartialMap, realMapY, Map.put(morePartialMap[realMapY], realMapX, chunk[yIndex][xIndex]))
          end)
        end)
    dechunk_map([restRow | rest], x + 1, y, newMap)
  end

  defp build_empty_map(size) do
    Enum.into(0..(size - 1), %{}, fn y -> {y, Enum.into(0..(size - 1), %{}, fn x-> {x, nil} end)} end)
  end

  defp flip_map_y(map) do
    for y <- 0..(map_size(map) - 1), into: %{}, do: {y, map[map_size(map) - y - 1]}
  end
  defp flip_map_x(map) do
    for y <- 0..(map_size(map) - 1), into: %{} do
      {y, (for x <- 0..(map_size(map) - 1), into: %{}, do: {x, map[y][map_size(map) - x - 1]})}
    end
  end

  defp rotate_map(map) do
    for y <- 0..(map_size(map) - 1), into: %{} do
      {y, for x <- 0..(map_size(map) - 1), into: %{} do {x, map[x][map_size(map) - y - 1]} end}
    end    
  end
end
