defmodule AdventOfCode2017.Day10 do
  import Bitwise
  @moduledoc """
  Day 10 solutions
  """

  @doc """
  Solves the first riddle of day 10.
  Examples:

      iex> AdventOfCode2017.Day10.solve("3, 4, 1, 5", 0..4)
      12
  """
  def solve(input, elements) do
    input
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.to_integer/1)
    |> knot_it(elements, 0, 0)
    |> elem(0)
    |> Enum.slice(0..1)
    |> Enum.reduce(&(&1 * &2))
  end

  @doc """
  Solves the second riddle of day 10. 
  Examples:

    iex> AdventOfCode2017.Day10.solve2("")
    "a2582a3a0e66e6e86e3812dcb672a272"

    iex> AdventOfCode2017.Day10.solve2("AoC 2017")
    "33efeb34ea91902bb2f59c9920caa6cd"

    iex> AdventOfCode2017.Day10.solve2("1,2,3")
    "3efbe78a8d82f29979031a4aa0b16a9d"

    iex> AdventOfCode2017.Day10.solve2("1,2,4")
    "63960835bcdc130f0b66d7ff4f6a5a8e"
  """
  def solve2(input) do
    lengths = String.to_charlist(input) ++ [17, 31, 73, 47, 23]
    Enum.reduce(1..64, {0..255, 0, 0}, 
    fn(_, {elements, startPos, startSkip}) -> 
      knot_it(lengths, elements, startPos, startSkip) end)
    |> elem(0)
    |> Enum.chunk_every(16)
    |> Enum.map(fn e -> Enum.reduce(e, &(&1 ^^^ &2)) end)
    |> Enum.map(fn s -> Integer.to_string(s, 16) |> String.pad_leading(2, "0") end)
    |> Enum.join
    |> String.downcase
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  def knot_it([curLen | lengths], elements, curPos, skipSize) do
    {start, mid} = Enum.split(elements, curPos)
    newElems = 
    Enum.reverse_slice(mid ++ start, 0, curLen) 
    |> Enum.split(-curPos)
    |> (fn {a, b} -> b ++ a end).()
    knot_it(lengths, newElems, move_pos(curPos, Enum.count(elements), curLen + skipSize), skipSize + 1)
  end
  def knot_it([], elements, curPos, curLen) do
    {elements, curPos, curLen}
  end

  defp move_pos(curPos, elemLen, move) when move > elemLen, do: move_pos(curPos, elemLen, rem(move, elemLen))
  defp move_pos(curPos, elemLength, move) when (curPos + move) < elemLength, do: curPos + move
  defp move_pos(curPos, elemLen, move), do: move - (elemLen - curPos)
end
