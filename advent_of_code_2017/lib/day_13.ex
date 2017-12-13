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

  def move_uncaught([]), do: :success
  def move_uncaught([%{scannerPos: 1} | _restLayers]), do: :caught
  def move_uncaught([_safeLayer | restLayers]) do
    scannersMoved = Enum.map(restLayers, &move_scanner/1)
    move_uncaught(scannersMoved)
  end

  defp try_move(layers) do
    host = self()
    Enum.map(0..9, 
      fn (step) ->
        spawn(
          fn -> 
            layersForStep = Enum.reduce(0..step, layers, fn (0, acc) -> acc; (_, acc) -> Enum.map(acc, &move_scanner/1) end)
            send(host, {step, move_uncaught(layersForStep), layersForStep})
          end)
      end)
    |> Enum.count
    |> collect_results([])    
  end
  
  defp collect_results(pids, results) do
    host = self()
    receive do
      {step, :success, _} -> success_found(pids - 1, [step | results])
      {step, :caught, layersForStep} ->
        spawn(fn ->
          compute = Enum.reduce(1..10, layersForStep, fn (_, acc) -> Enum.map(acc, &move_scanner/1) end)
          send(host, {step + 10, move_uncaught(compute), compute})
        end)
        collect_results(pids, results)      
    end
  end
  
  defp success_found(0, results), do: Enum.min(results)
  defp success_found(pids, results) do
    receive do
      {step, :success, _} -> success_found(pids - 1, [step | results])
      {_step, :caught, _layersForStep} -> success_found(pids - 1, results)    
    end
  end

  defp move_scanner(%{scannerPos: 1} = layer), do: %{layer | scannerPos: 2, scannerDirection: :down} 
  defp move_scanner(%{scannerPos: range, range: range} = layer), do: %{layer | scannerPos: range - 1, scannerDirection: :up}
  defp move_scanner(%{scannerPos: scannerPos, scannerDirection: :down} = layer), do: %{layer | scannerPos: scannerPos + 1, scannerDirection: :down}
  defp move_scanner(%{scannerPos: scannerPos, scannerDirection: :up} = layer), do: %{layer | scannerPos: scannerPos - 1, scannerDirection: :up}
  defp move_scanner(emptyLayer), do: emptyLayer
end
