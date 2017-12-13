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
    |> move(0)
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
    create_layer(restLayers, layerIndex + 1, layers ++ [%{index: layerIndex, range: layerRange, scannerPos: 1, scannerDirection: :down}])
  end
  defp create_layer(restLayers, layerIndex, layers) do
    create_layer(restLayers, layerIndex + 1, layers ++ [%{index: layerIndex, range: :empty}])
  end

  defp move([], severity), do: severity
  defp move([%{range: :empty} | restLayers], severity) do
    scannersMoved = Enum.map(restLayers, &move_scanner/1)
    move(scannersMoved, severity)
  end
  defp move([%{scannerPos: 1, range: range, index: index} | restLayers], severity) do
    scannersMoved = Enum.map(restLayers, &move_scanner/1)
    move(scannersMoved, severity + index * range)    
  end
  defp move([_safeLayer | restLayers], severity) do
    scannersMoved = Enum.map(restLayers, &move_scanner/1)
    move(scannersMoved, severity)
  end

  defp move_uncaught([]), do: :success
  defp move_uncaught([%{scannerPos: 1} | _restLayers]), do: :caught
  defp move_uncaught([_safeLayer | restLayers]) do
    scannersMoved = Enum.map(restLayers, &move_scanner/1)
    move_uncaught(scannersMoved)
  end

  defp try_move(layers, delay \\ 0) do
    case move_uncaught(layers) do
      :success -> delay
      :caught -> try_move(Enum.map(layers, &move_scanner/1), delay + 1)
    end
  end

  defp move_scanner(%{scannerPos: 1} = layer), do: %{layer | scannerPos: 2, scannerDirection: :down} 
  defp move_scanner(%{scannerPos: range, range: range} = layer), do: %{layer | scannerPos: range - 1, scannerDirection: :up}
  defp move_scanner(%{scannerPos: scannerPos, scannerDirection: :down} = layer), do: %{layer | scannerPos: scannerPos + 1, scannerDirection: :down}
  defp move_scanner(%{scannerPos: scannerPos, scannerDirection: :up} = layer), do: %{layer | scannerPos: scannerPos - 1, scannerDirection: :up}
  defp move_scanner(emptyLayer), do: emptyLayer
end
