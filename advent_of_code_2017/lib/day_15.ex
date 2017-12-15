defmodule AdventOfCode2017.Day15 do
  @moduledoc """
  Day 15 solutions
  """

  @doc """
  Solves the first riddle of day 15.
  Examples:

      iex> AdventOfCode2017.Day15.solve(5, 65, 8921)
      1

      iex> AdventOfCode2017.Day15.solve(40000000, 65, 8921)
      588

  """
  def solve(input, startA, startB) do
    gen_step(input, startA, startB, 0)
  end

  @doc """
  Solves the second riddle of day 15. 
  Examples:

      iex> AdventOfCode2017.Day15.solve2(1056, 65, 8921)
      1

      iex> AdventOfCode2017.Day15.solve2(5000000, 65, 8921)
      309
  """
  def solve2(input, startA, startB) do
    gen_picky_step(input, startA, startB, 0)
    # Process.register(spawn(__MODULE__, :judge_proc, [input, [], [], 0, self()]), :judge)
    # spawn(__MODULE__, :gen_proc, [:a, 16807, 4, startA])
    # spawn(__MODULE__, :gen_proc, [:b, 48271, 8, startB])
    # receive do
    #   {:score, score} -> score
    # end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # ------------------------------------------------------------------
  defp gen_step(0, _, _, judgeCount), do: judgeCount
  defp gen_step(n, genAVal, genBVal, judgeCount) do
    newAVal = rem(genAVal * 16807, 2147483647)
    newBVal = rem(genBVal * 48271, 2147483647)
    case << newAVal :: size(16) >> == << newBVal :: size(16) >> do
      true -> gen_step(n - 1, newAVal, newBVal, judgeCount + 1)
      false -> gen_step(n - 1, newAVal, newBVal, judgeCount)
    end
  end

  defp gen_picky_step(0, _, _, judgeCount), do: judgeCount
  defp gen_picky_step(n, aVal, bVal, judgeCount) do
    newAVal = make_value(aVal, 16807, 4)
    newBVal = make_value(bVal, 48271, 8)
    case << newAVal :: size(16) >> == << newBVal :: size(16) >> do
      true -> gen_picky_step(n - 1, newAVal, newBVal, judgeCount + 1)
      false -> gen_picky_step(n - 1, newAVal, newBVal, judgeCount)
    end
  end

  defp make_value(val, multiplier, remVal) do
    newVal = rem(val * multiplier, 2147483647)
    case rem(newVal, remVal) == 0 do
      true -> newVal
      false -> make_value(newVal, multiplier, remVal)
    end
  end
end
