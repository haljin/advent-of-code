defmodule AdventOfCode2017.Day9 do
  @moduledoc """
  Day 9 solutions
  """

  @doc """
  Solves the first riddle of day 9.
  Examples:

  iex> AdventOfCode2017.Day9.solve("{}")
  1

  iex> AdventOfCode2017.Day9.solve("{{{}}}")
  6

  iex> AdventOfCode2017.Day9.solve("{{},{}}")
  5

  iex> AdventOfCode2017.Day9.solve("{{{},{},{{}}}}")
  16

  iex> AdventOfCode2017.Day9.solve("{<a>,<a>,<a>,<a>}")
  1

  iex> AdventOfCode2017.Day9.solve("{{<ab>},{<ab>},{<ab>},{<ab>}}")
  9

  iex> AdventOfCode2017.Day9.solve("{{<!!>},{<!!>},{<!!>},{<!!>}}")
  9

  iex> AdventOfCode2017.Day9.solve("{{<a!>},{<a!>},{<a!>},{<ab>}}")
  3
  """
  def solve(input) do
    input
    |> String.codepoints
    |> count_groups(0,0, false)
  end

  @doc """
  Solves the second riddle of day 9. 
  Examples:


  iex> AdventOfCode2017.Day9.solve2("<>")
  0

  iex> AdventOfCode2017.Day9.solve2("<random characters>")
  17

  iex> AdventOfCode2017.Day9.solve2("<<<<>")
  3

  iex> AdventOfCode2017.Day9.solve2("<{!>}>")
  2

  iex> AdventOfCode2017.Day9.solve2("<!!>")
  0

  iex> AdventOfCode2017.Day9.solve2("<!!!>>")
  0

  iex> AdventOfCode2017.Day9.solve2(~s(<{o"i!a,<{i<a>))
  10
  """
  def solve2(input) do
    input
    |> String.codepoints
    |> count_garbage(0, false)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp count_groups(["!", _toDrop | rest], level, score, inGarbage), do: count_groups(rest, level, score, inGarbage)
  defp count_groups(["{" | rest], level, score, false), do: count_groups(rest, level + 1, score, false)
  defp count_groups(["}" | rest], level, score, false), do: count_groups(rest, level - 1, score + level, false)
  defp count_groups(["<" | rest], level, score, false), do: count_groups(rest, level, score, true)
  defp count_groups([">" | rest], level, score, true), do: count_groups(rest, level, score, false)
  defp count_groups([_someChar | rest], level, score, inGarbage), do: count_groups(rest, level, score, inGarbage)
  defp count_groups([], 0, score, false), do: score

  defp count_garbage(["!", _toDrop | rest], score, inGarbage), do: count_garbage(rest, score, inGarbage)
  defp count_garbage(["<" | rest], score, false), do: count_garbage(rest, score, true)
  defp count_garbage([">" | rest], score, true), do: count_garbage(rest, score, false)
  defp count_garbage([_someChar | rest], score, true), do: count_garbage(rest, score + 1, true)
  defp count_garbage([_someChar | rest], score, false), do: count_garbage(rest, score, false)
  defp count_garbage([], score, false), do: score

end
