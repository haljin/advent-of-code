defmodule AdventOfCode2017.Day1 do
  @moduledoc """
  Day 1 solutions
  """

  @doc """
  Solves the first riddle of day 1. 
  Examples:

      iex> AdventOfCode2017.Day1.solve("1122")
      3

      iex> AdventOfCode2017.Day1.solve("1111")
      4

      iex> AdventOfCode2017.Day1.solve("1234")
      0

      iex> AdventOfCode2017.Day1.solve("91212129")
      9

  """
  def solve(input) do
    input |>
    String.codepoints |>
    Enum.map(&String.to_integer/1) |>
    do_solve(1)
  end

  @doc """
  Solves the second riddle of day . 
  Examples:

      iex> AdventOfCode2017.Day1.solve2("1212")
      6

      iex> AdventOfCode2017.Day1.solve2("1221")
      0

      iex> AdventOfCode2017.Day1.solve2("123425")
      4

      iex> AdventOfCode2017.Day1.solve2("123123")
      12

      iex> AdventOfCode2017.Day1.solve2("12131415")
      4
  """
  def solve2(input) do
    input |>
    String.codepoints |>
    Enum.map(&String.to_integer/1) |>
    do_solve((div String.length(input), 2))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp do_solve(digits, steps) do
    do_solve(steps, digits, digits, 0)
  end
  defp do_solve(_steps, [], _full, sum) do
    sum
  end
  defp do_solve(steps, [digit | rest], full, sum) do
    case find_next(steps, rest, full) do
      ^digit -> do_solve(steps, rest, full, sum + digit)
      _ -> do_solve(steps, rest, full, sum)
    end
  end

  defp find_next(1, [digit | _rest], _full) do
    digit
  end
  defp find_next(steps, [_digit | rest], full) do
    find_next(steps - 1, rest, full)
  end
  defp find_next(steps, [], full) do
    find_next(steps, full, full)
  end
end
