defmodule AdventOfCode2017.Day11 do
  @moduledoc """
  Day 11 solutions
  """

  @doc """
  Solves the first riddle of day 11.
  Examples:
    iex> AdventOfCode2017.Day11.solve("ne,ne,ne")
    3 
    
    iex> AdventOfCode2017.Day11.solve("ne,ne,sw,sw")
    0
    
    iex> AdventOfCode2017.Day11.solve("ne,ne,s,s") 
    2
    
    iex> AdventOfCode2017.Day11.solve("se,sw,se,sw,sw")
    3

  """
  def solve(input) do
    input
    |> String.split(",")
    |> Enum.map(&String.to_atom/1)
    |> go(0, 0, 0)
    |> elem(0)
    |> hex_dist
  end

  @doc """
  Solves the second riddle of day 11. 
  """
  def solve2(input) do
    input
    |> String.split(",")
    |> Enum.map(&String.to_atom/1)
    |> go(0, 0, 0)
    |> elem(1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # ------------------------------------------------------------------
  defp hex_dist({ax, ay, az}, {bx, by, bz} \\ {0, 0, 0}), do: Enum.max([abs(ax - bx), abs(ay - by), abs(az - bz)])

  defp go(steps, x, y, z, furthest \\ 0)
  defp go([], x, y, z, furthest), do: {{x, y, z}, furthest}
  defp go([:n | rest], x, y, z, furthest), do: go(rest, x, y + 1, z - 1, max(furthest, hex_dist({x, y, z})))
  defp go([:s | rest], x, y, z, furthest), do: go(rest, x, y - 1, z + 1, max(furthest, hex_dist({x, y, z})))
  defp go([:ne | rest], x, y, z, furthest), do: go(rest, x + 1, y, z - 1, max(furthest, hex_dist({x, y, z})))
  defp go([:sw | rest], x, y, z, furthest), do: go(rest, x - 1, y, z + 1, max(furthest, hex_dist({x, y, z})))
  defp go([:nw | rest], x, y, z, furthest), do: go(rest, x - 1, y + 1, z, max(furthest, hex_dist({x, y, z})))
  defp go([:se | rest], x, y, z, furthest), do: go(rest, x + 1, y - 1, z, max(furthest, hex_dist({x, y, z})))
end
