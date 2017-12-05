defmodule AdventOfCode2017.Day4 do
  @moduledoc """
  Day 4 solutions
  """

  @doc """
  Solves the first riddle of day 4.
  Examples:

      iex> AdventOfCode2017.Day4.solve("aa bb cc dd ee")
      1
      
      iex> AdventOfCode2017.Day4.solve("aa bb cc dd aa")
      0

      iex> AdventOfCode2017.Day4.solve("aa bb cc dd aaa")
      1
  """
  def solve(input) do
    input 
    |> String.split("\n")
    |> Enum.map(&String.split/1)
    |> zip_with_uniques
    |> Enum.filter(fn {a, b} -> length(a) == length(b) end ) 
    |> length
  end

  @doc """
  Solves the second riddle of day 4. 
  Examples:

      iex> AdventOfCode2017.Day4.solve2("abcde fghij")
      1

      iex> AdventOfCode2017.Day4.solve2("abcde xyz ecdab")
      0

      iex> AdventOfCode2017.Day4.solve2("a ab abc abd abf abj")
      1

      iex> AdventOfCode2017.Day4.solve2("iiii oiii ooii oooi oooo")
      1

      iex> AdventOfCode2017.Day4.solve2("oiii ioii iioi iiio")
      0
  """
  def solve2(input) do
    input 
    |> String.split("\n")
    |> Enum.map(&String.split/1)
    |> Enum.map(fn pass -> Enum.map(pass, &sort_strings/1) end)
    |> zip_with_uniques
    |> Enum.filter(fn {a, b} -> length(a) == length(b) end ) 
    |> length
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp zip_with_uniques(list) do
    list 
    |> Enum.map(&Enum.uniq/1)
    |> Enum.zip(list)
  end

  defp sort_strings(string) do
    string 
    |> String.codepoints 
    |> Enum.sort
  end

end
