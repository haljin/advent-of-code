defmodule AdventOfCode2017.Day25 do
  @moduledoc """
  Day 25 solutions
  """
  defmodule TuringMachine do
    defstruct start_state: nil, steps: 0, states: []
  end

  @doc """
  Solves the first riddle of day 25.

      iex> AdventOfCode2017.Day25.solve("Begin in state A.
      ...> Perform a diagnostic checksum after 6 steps.
      ...> 
      ...> In state A:
      ...>  If the current value is 0:
      ...>     - Write the value 1.
      ...>     - Move one slot to the right.
      ...>     - Continue with state B.
      ...>   If the current value is 1:
      ...>     - Write the value 0.
      ...>     - Move one slot to the left.
      ...>     - Continue with state B.
      ...> 
      ...> In state B:
      ...>   If the current value is 0:
      ...>     - Write the value 1.
      ...>     - Move one slot to the left.
      ...>     - Continue with state A.
      ...>   If the current value is 1:
      ...>     - Write the value 1.
      ...>     - Move one slot to the right.
      ...>     - Continue with state A.")
      3
  """
  def solve(input) do
    input
    |> parse
    |> (fn machine -> operate(machine, machine.start_state, 0, %{}) end).()
    |> Map.values
    |> Enum.sum
  end

  @doc """
  Solves the second riddle of day 25. 
  """
  def solve2(input) do
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp operate(%TuringMachine{steps: 0}, _curState, _curPos, tape), do: tape
  defp operate(%TuringMachine{steps: steps} = machine, curState, curPos, tape) do
    transitionInfo = machine.states[curState]
    curValue = Map.get(tape, curPos, 0)
    {^curValue, toWrite, direction, nextState} = List.keyfind(transitionInfo, curValue, 0)
    operate(%TuringMachine{machine | steps: steps - 1}, nextState, move(direction, curPos), Map.put(tape, curPos, toWrite))
  end

  defp move(:left, pos), do: pos - 1
  defp move(:right, pos), do: pos + 1


  defp parse(input) do
    String.split(input, "\n")
    |> Enum.map(&String.trim/1)
    |> build_machine(%TuringMachine{})
  end

  defp build_machine([], struct), do: struct
  defp build_machine(["" | rest], struct), do: build_machine(rest, struct)
  defp build_machine(["Begin in state " <> <<state :: binary-size(1) >> <> "." | rest], struct) do
    build_machine(rest, %TuringMachine{struct | start_state: String.to_atom(state)})
  end
  defp build_machine(["Perform a diagnostic checksum after " <> steps | rest], struct) do
    {steps, _} = Integer.parse(steps)
    build_machine(rest, %TuringMachine{struct | steps: steps})
  end
  defp build_machine(["In state " <> <<state :: binary-size(1) >> <> ":" | rest], struct) do
    build_state(rest, struct, String.to_atom(state), [])
  end

  defp build_state(["If the current value is " <> <<value :: binary-size(1) >> <> ":" | restWithStateInfo], struct, curState, curStateData) do
    [
      "- Write the value " <> <<valueToWrite :: binary-size(1) >> <> ".",
      "- Move one slot to the " <> move,
      "- Continue with state " <> <<nextState :: binary-size(1) >> <> "."
      | rest
      ] = restWithStateInfo
      move = String.replace(move, ".", "")
      transitionInfo = {String.to_integer(value), String.to_integer(valueToWrite), String.to_atom(move), String.to_atom(nextState)}
      build_state(rest, struct, curState, [transitionInfo | curStateData])
  end
  defp build_state(rest, %TuringMachine{states: prevStates} = struct, curState,  curStateData) do
    build_machine(rest, %TuringMachine{struct | states: prevStates ++ [{curState, curStateData}]})
  end
end
