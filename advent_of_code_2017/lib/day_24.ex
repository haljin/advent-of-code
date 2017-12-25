defmodule AdventOfCode2017.Day24 do
  @moduledoc """
  Day 24 solutions
  """

  @doc """
  Solves the first riddle of day 24.

    iex> AdventOfCode2017.Day24.solve("0/2
    ...> 2/2
    ...> 2/3
    ...> 3/4
    ...> 3/5
    ...> 0/1
    ...> 10/1
    ...> 9/10")
    31
  """
  def solve(input) do
    input
    |> parse
    |> build_tree
    |> find_strongest
  end

  @doc """
  Solves the second riddle of day 24. 

    iex> AdventOfCode2017.Day24.solve2("0/2
    ...> 2/2
    ...> 2/3
    ...> 3/4
    ...> 3/5
    ...> 0/1
    ...> 10/1
    ...> 9/10")
    19
  """
  def solve2(input) do
    input
    |> parse
    |> build_tree
    |> find_longest_strongest
    |> elem(1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp parse(input) do
    String.split(input, "\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&(String.split(&1, "/")))
    |> Enum.map(fn [a, b] -> {String.to_integer(a), String.to_integer(b)} end)
  end

  defp build_tree(parts), do: {:node, {0, 0}, build_tree(parts, 0)}
  defp build_tree(parts, port) do
    Enum.filter(parts, fn {a, b} -> a == port or b == port end)
    |> Enum.map(&({:node, &1, build_tree(parts -- [&1], other_port(&1, port))}))
  end

  defp find_strongest({:node, {a, b}, []}), do: a + b
  defp find_strongest({:node, {a, b}, children}) do
    Enum.map(children, &find_strongest/1)
    |> Enum.max
    |> (fn maxChild -> maxChild + a + b end).()
  end

  defp find_longest_strongest({:node, {a, b}, []}), do: {0, a + b}
  defp find_longest_strongest({:node, {a, b}, children}) do
    Enum.map(children, &find_longest_strongest/1)
    |> longest_bridges
    |> Enum.max_by(fn {_, score} -> score end)
    |> (fn {maxLen, maxScore} -> {maxLen + 1, maxScore + a + b} end).()
  end

  defp longest_bridges(list) do
    {maxLen, _} = Enum.max_by(list, fn {len, _} -> len end)
    Enum.filter(list, fn {len, _} -> len == maxLen end)
  end

  defp other_port({port, other}, port), do: other
  defp other_port({other, port}, port), do: other
  
end
