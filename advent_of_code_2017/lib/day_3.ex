defmodule AdventOfCode2017.Day3 do
  @moduledoc """
  Day 3 solutions
  """

  @doc """
  Solves the first riddle of day 3.
  Examples:

      iex> AdventOfCode2017.Day3.solve(1)
      0

      iex> AdventOfCode2017.Day3.solve(12)
      3

      iex> AdventOfCode2017.Day3.solve(23)
      2

      iex> AdventOfCode2017.Day3.solve(1024)
      31
  """
  def solve(input) do
    build_spiral(:left, {0, 0}, {1, -1, 1, -1}, build_path_fun(1, input))
  end

  @doc """
  Solves the second riddle of day 3. 
  Examples:

      iex> AdventOfCode2017.Day3.solve2(4)
      5

      iex> AdventOfCode2017.Day3.solve2(7)
      10

      iex> AdventOfCode2017.Day3.solve2(100)
      122

      iex> AdventOfCode2017.Day3.solve2(300)
      304
  """
  def solve2(input) do
    build_spiral(:left, {0, 0}, {1, -1, 1, -1}, build_neighbour_sum_fun(%{{0,0} => 1}, input))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp build_spiral(dir, {curX, curY}, boundaries, validateFun) do
    case validateFun.(curX, curY) do
      {:done, val} -> val
      newValidateFun -> make_spiral(dir, {curX, curY}, boundaries, newValidateFun)
    end
  end

  defp make_spiral(:left, {maxX, curY}, {maxX, minX, maxY, minY}, validateFun) do
    build_spiral(:up, {maxX, curY + 1}, {maxX + 1, minX, maxY, minY}, validateFun)
  end
  defp make_spiral(:left, {curX, curY}, boundaries, validateFun) do
    build_spiral(:left, {curX + 1, curY}, boundaries, validateFun)    
  end
  defp make_spiral(:up, {curX, maxY}, {maxX, minX, maxY, minY}, validateFun) do
    build_spiral(:right, {curX - 1, maxY}, {maxX, minX, maxY + 1, minY}, validateFun)
  end
  defp make_spiral(:up, {curX, curY}, boundaries, validateFun) do
    build_spiral(:up, {curX, curY + 1}, boundaries, validateFun)
  end
  defp make_spiral(:right , {minX, curY}, {maxX, minX, maxY, minY}, validateFun) do
    build_spiral(:down, {minX, curY - 1}, {maxX, minX - 1, maxY, minY}, validateFun) 
  end
  defp make_spiral(:right, {curX, curY}, boundaries, validateFun) do
    build_spiral(:right, {curX - 1, curY}, boundaries, validateFun)
  end
  defp make_spiral(:down, {curX, minY}, {maxX, minX, maxY, minY}, validateFun) do
    build_spiral(:left, {curX + 1, minY}, {maxX, minX, maxY, minY - 1}, validateFun) 
  end
  defp make_spiral(:down, {curX, curY}, boundaries, validateFun) do
    build_spiral(:down, {curX, curY - 1}, boundaries, validateFun)
  end

  defp build_path_fun(finalN, finalN) do
    fn (curX, curY) ->
        {:done, abs(curX) + abs(curY)}
    end
  end
  defp build_path_fun(n, finalN) do
    fn (_, _) ->
      build_path_fun(n + 1, finalN)    
    end
  end

  defp build_neighbour_sum_fun(spiralMap, solution) do
    fn(curX, curY) ->
      mySum =
      (for x <- (curX - 1)..(curX + 1), y <- (curY - 1)..(curY + 1), do: spiralMap[{x, y}]) 
      |> Enum.filter(&(&1 != nil)) 
      |> Enum.sum
      if mySum > solution do
        {:done, mySum}
      else
        build_neighbour_sum_fun(Map.put(spiralMap, {curX, curY}, mySum), solution)
      end    
    end
  end
  
end
