defmodule AdventOfCode2017.Day16 do
  @moduledoc """
  Day 16 solutions
  """

  @doc """
  Solves the first riddle of day 16.
  Examples:

      iex> AdventOfCode2017.Day16.solve("s1,x3/4,pe/b", 'abcde')
      "baedc"

  """
  def solve(input, dancers \\ 'abcdefghijklmnop') do
    input
    |> String.split(",")
    |> Enum.map(&parse_op/1)
    |> dance(dancers)
    |> List.to_string
  end

  @doc """
  Solves the second riddle of day 16. 

      iex> AdventOfCode2017.Day16.solve2("s1,x3/4,pe/b", 'abcde', 2)
      "ceadb"
  """
  def solve2(input, dancers \\ 'abcdefghijklmnop', times \\ 1000000000) do
    input
    |> String.split(",")
    |> Enum.map(&parse_op/1)
    |> find_pattern(dancers, times)
    |> (fn {n, dance} -> dance_times(dance, dancers, n) end).()
    |> List.to_string

  end

  # -------------------------------------------------------------------
  # Internal functions
  # ------------------------------------------------------------------
  defp dance([], dancers), do: dancers 
  defp dance([{:spin, pos} | rest], dancers) do
    {pre, pos} = Enum.split(dancers, -pos)
    dance(rest, pos ++ pre)
  end
  defp dance([{:exchange, posA, posB} | rest], dancers) do
    newDancers =
    dancers
    |> Enum.with_index
    |> Enum.map(fn {_, ^posA} -> Enum.at(dancers, posB); {_, ^posB} -> Enum.at(dancers, posA); {el, _} -> el end)
    dance(rest, newDancers)
  end
  defp dance([{:partner, a, b} | rest], dancers) do
    newDancers = Enum.map(dancers, fn ^a -> b; ^b -> a; other -> other end)
    dance(rest, newDancers)    
  end

  defp find_pattern(dance, dancers, times, n \\ 0, seen \\ []) do
    case dancers in seen do
      true -> {rem(times, n), dance}
      false -> find_pattern(dance, dance(dance, dancers), times, n + 1, [dancers | seen])
    end    
  end

  defp dance_times(_, dancers, 0), do: dancers
  defp dance_times(dance, dancers, n) do
    dance_times(dance, dance(dance, dancers), n - 1)
  end

  defp parse_op("s" <> number), do: {:spin, String.to_integer(number)}
  defp parse_op("x" <> processes) do
    [a, b] = String.split(processes, "/") 
    {:exchange, String.to_integer(a), String.to_integer(b)}
  end
  defp parse_op("p" <> processes) do
    [a, b] = String.split(processes, "/") 
    {:partner, hd(String.to_charlist(a)), hd(String.to_charlist(b))}  
  end
  
end
