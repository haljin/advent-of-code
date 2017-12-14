defmodule AdventOfCode2017.Day14 do
  @moduledoc """
  Day 14 solutions
  """

  @doc """
  Solves the first riddle of day 14.
  Examples:

    iex> AdventOfCode2017.Day14.solve("flqrgnkx")
    8108
  """
  def solve(input) do
    input
    |> make_strings
    |> Enum.map(&AdventOfCode2017.Day10.solve2/1)
    |> Enum.map(&hex_string_to_binary/1)
    |> Enum.map(&to_row/1)
    |> Enum.map(&(Enum.sum(&1)))
    |> Enum.sum
  end

  @doc """
  Solves the second riddle of day 14. 
  Examples:

    iex> AdventOfCode2017.Day14.solve2("flqrgnkx")
    1242
  """
  def solve2(input) do
    input
    |> make_strings
    |> Enum.map(&AdventOfCode2017.Day10.solve2/1)
    |> Enum.map(&hex_string_to_binary/1)
    |> Enum.map(&to_row/1)
    |> Enum.map(&(&1 |> Enum.with_index |> Enum.into(%{}, fn {v, k} -> {k, v} end)))
    |> Enum.with_index 
    |> Enum.into(%{}, fn {v, k} -> {k, v} end)
    |> walk_map(0, 0, region_map(), 0)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # ------------------------------------------------------------------
  defp make_strings(input) do
    Enum.map(0..127, &("#{input}-#{&1}"))
  end

  defp hex_string_to_binary(string), do: hex_string_to_binary(String.codepoints(string), <<>>)
  defp hex_string_to_binary([], binary), do: binary
  defp hex_string_to_binary([char | rest], binary) do
    hex_string_to_binary(rest, << binary :: bitstring, String.to_integer(char, 16) :: size(4) >>)
  end

  defp to_row(binary, row \\ [])
  defp to_row(<<1 :: size(1), rest :: bitstring>>, row), do: to_row(rest, row ++ [1])
  defp to_row(<<0 :: size(1), rest :: bitstring>>, row), do: to_row(rest, row ++ [0])
  defp to_row(<<>>, row), do: row

  defp walk_map(_map, _, 128, _regionMap, regions), do: regions
  defp walk_map(map, 128, y, regionMap, regions) do
    walk_map(map, 0, y + 1, regionMap, regions)
  end    
  defp walk_map(map, x, y, regionMap, regions) do
    {newRegionmap, newRegions} = 
    case {map[x][y], regionMap[x][y]} do
      {0, _} -> {regionMap, regions}
      {1, true} -> {regionMap, regions}
      {1, false} -> 
        {find_region(regionMap, map, x, y), regions + 1}
    end
    walk_map(map, x + 1, y, newRegionmap, newRegions)
  end

  defp find_region(regionMap, map, x, y) do
    case {map[x][y], regionMap[x][y]} do
      {nil, nil} -> regionMap
      {_, true} -> regionMap
      {0, _} -> regionMap
      {1, _} -> 
        %{regionMap | x => %{regionMap[x] | y => true}}
        |> find_region(map, x + 1, y)
        |> find_region(map, x - 1, y)
        |> find_region(map, x, y + 1)
        |> find_region(map, x, y - 1)
    end

  end

  defp region_map do
    Enum.into(0..127, %{}, fn x -> {x, Enum.into(0..127, %{}, fn i -> {i, false} end)} end)
  end
end
