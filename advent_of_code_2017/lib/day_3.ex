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
    build_spiral(:left, {0, 0}, {1, -1, 1, -1}, &path_fun/3, {1, input})
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
    build_spiral(:left, {0, 0}, {1, -1, 1, -1}, &neighbour_sum_fum/3, {%{{0,0} => 1}, input})
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp build_spiral(dir, {curX, curY}, boundaries, validateFun, data) do
    case validateFun.(curX, curY, data) do
      {:done, val} -> val
      newData -> make_spiral(dir, {curX, curY}, boundaries, validateFun, newData)
    end
  end

  defp make_spiral(:left, {maxX, curY}, {maxX, minX, maxY, minY}, validateFun, data) do
    build_spiral(:up, {maxX, curY + 1}, {maxX + 1, minX, maxY, minY}, validateFun, data)
  end
  defp make_spiral(:left, {curX, curY}, boundaries, validateFun, data) do
    build_spiral(:left, {curX + 1, curY}, boundaries, validateFun, data)    
  end
  defp make_spiral(:up, {curX, maxY}, {maxX, minX, maxY, minY}, validateFun, data) do
    build_spiral(:right, {curX - 1, maxY}, {maxX, minX, maxY + 1, minY}, validateFun, data)
  end
  defp make_spiral(:up, {curX, curY}, boundaries, validateFun, data) do
    build_spiral(:up, {curX, curY + 1}, boundaries, validateFun, data)
  end
  defp make_spiral(:right , {minX, curY}, {maxX, minX, maxY, minY}, validateFun, data) do
    build_spiral(:down, {minX, curY - 1}, {maxX, minX - 1, maxY, minY}, validateFun, data) 
  end
  defp make_spiral(:right, {curX, curY}, boundaries, validateFun, data) do
    build_spiral(:right, {curX - 1, curY}, boundaries, validateFun, data)
  end
  defp make_spiral(:down, {curX, minY}, {maxX, minX, maxY, minY}, validateFun, data) do
    build_spiral(:left, {curX + 1, minY}, {maxX, minX, maxY, minY - 1}, validateFun, data) 
  end
  defp make_spiral(:down, {curX, curY}, boundaries, validateFun, data) do
    build_spiral(:down, {curX, curY - 1}, boundaries, validateFun, data)
  end

  defp path_fun(curX, curY, {finalN, finalN}) do
    {:done, abs(curX) + abs(curY)}
  end
  defp path_fun(_, _, {n, finalN}) do
    {n + 1, finalN}
  end

  defp neighbour_sum_fum(curX, curY, {spiralMap, solution}) do
    mySum =
    (for x <- (curX - 1)..(curX + 1), y <- (curY - 1)..(curY + 1), do: spiralMap[{x, y}]) |>
    Enum.filter(&(&1 != nil)) |>
    Enum.sum
    if mySum > solution do
      {:done, mySum}
    else
      {Map.put(spiralMap, {curX, curY}, mySum), solution}
    end
  end
end
