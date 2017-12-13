defmodule AdventOfCode2017.Day13 do
  @moduledoc """
  Day 13 solutions
  """

  @doc """
  Solves the first riddle of day 13.
  Examples:

    iex> AdventOfCode2017.Day13.solve("0: 3
    ...> 1: 2
    ...> 4: 4
    ...> 6: 4")
    24
  """
  def solve(input) do
    input
    |> parse
    |> create_layer(0, [])
    |> move
  end

  @doc """
  Solves the second riddle of day 13. 
  Examples:

    iex> AdventOfCode2017.Day13.solve2("0: 3
    ...> 1: 2
    ...> 4: 4
    ...> 6: 4")
    10
  """
  def solve2(input) do
    input
    |> parse
    |> create_layer(0, [])
    |> try_move
  end

  # -------------------------------------------------------------------
  # Internal functions
  # ------------------------------------------------------------------
  defp parse(input) do
    input
    |> String.split("\n")
    |> Enum.map(&(String.split(&1, ": ")))
    |> Enum.map(fn [layer, range] -> {String.trim(layer) |> String.to_integer, String.to_integer(range)} end)
  end

  defp create_layer([], _, layers), do: layers
  defp create_layer([{layerIndex, layerRange} | restLayers], layerIndex, layers) do
    create_layer(restLayers, layerIndex + 1, layers ++ [%{index: layerIndex, range: layerRange}])
  end
  defp create_layer(restLayers, layerIndex, layers) do
    create_layer(restLayers, layerIndex + 1, layers ++ [%{index: layerIndex, range: :empty}])
  end

  defp move(layers, severity \\ 0, time \\ 0)
  defp move([], severity, _time), do: severity
  defp move([%{range: :empty} | restLayers], severity, time) do
    move(restLayers, severity, time + 1)
  end
  defp move([%{range: range, index: index} | restLayers], severity, time) when rem(time, ((range - 1) *2)) == 0 do
    move(restLayers, severity + index * range, time + 1)    
  end
  defp move([_safeLayer | restLayers], severity, time) do
    move(restLayers, severity, time + 1)
  end

  defp move_uncaught([], _time), do: :success
  defp move_uncaught([%{range: range} | _restLayers], time) when rem(time, ((range - 1) *2)) == 0, do: :caught
  defp move_uncaught([_safeLayer | restLayers], time) do
    move_uncaught(restLayers, time + 1)
  end

  defp try_move(layers, delay \\ 0) do
    case move_uncaught(layers, delay) do
      :success -> delay
      :caught -> try_move(layers, delay + 1)
    end
  end
end
