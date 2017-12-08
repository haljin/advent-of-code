defmodule AdventOfCode2017.Day8 do
  @moduledoc """
  Day 8 solutions
  """

  @doc """
  Solves the first riddle of day 8.
  Examples:

      iex> AdventOfCode2017.Day8.solve("b inc 5 if a > 1
      ...> a inc 1 if b < 5
      ...> c dec -10 if a >= 1
      ...> c inc -20 if c == 10")
      1



  """
  def solve(input) do
    input
    |> parse_input
    |> Enum.map(&sendOp/1)
    |> Enum.uniq
    |> Enum.map(fn reg -> send(reg, {:get_val, self()}); receive do regVal -> {reg, regVal} end end)
    |> Enum.map(&(elem(&1, 1)))
    |> Enum.max
  end

  @doc """
  Solves the second riddle of day 8. 
  Examples:

      iex> AdventOfCode2017.Day8.solve2("b inc 5 if a > 1
      ...> a inc 1 if b < 5
      ...> c dec -10 if a >= 1
      ...> c inc -20 if c == 10")
      10



  """
  def solve2(input) do
    Process.register(spawn(__MODULE__, :coord_proc, [nil]), :coordinator)
    input
    |> parse_input
    |> Enum.map(&sendOp/1)
    |> Enum.uniq
    |> Enum.map(fn reg -> send(reg, {:get_val, self()}); receive do regVal -> {reg, regVal} end end)
    send(:coordinator, {:get, self()})
    receive do max -> max end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  def register_proc(myName, currentVal) do
    receive do
      {:check, condOp, condVal, pid} ->
        reply = apply(Kernel, condOp, [currentVal, condVal])
        send(pid, {:result, reply})
        register_proc(myName, currentVal)
      {op, val, condition, pid} ->
        newVal = 
        case check_condition(myName, currentVal, condition) do
          true -> applyOp(op, val, currentVal)
          false -> currentVal          
        end
        send(pid, :ok)
        if Process.whereis(:coordinator) != nil, do: send(:coordinator, {:val, newVal})
        register_proc(myName, newVal)
      {:get_val, pid} ->
        send(pid, currentVal)
    end    
  end

  def coord_proc(curVal) do
    receive do
      {:val, val} when (val > curVal) or (curVal == nil) -> coord_proc(val)
      {:val, _} -> coord_proc(curVal)
      {:get, pid} -> send(pid, curVal)
    end
  end

  defp sendOp({reg, op, val, condition}) do    
    if Process.whereis(reg) == nil, do: Process.register(spawn(__MODULE__, :register_proc, [reg, 0]), reg)
    send(reg, {op, val, condition, self()})
    receive do :ok -> :ok end
    reg
  end

  defp applyOp(:dec, value, curValue), do: curValue - value
  defp applyOp(:inc, value, curValue), do: curValue + value

  defp check_condition(myName, currentVal, {myName, condOp, condVal}) do
    apply(Kernel, condOp, [currentVal, condVal])
  end
  defp check_condition(_, _currentVal, {condReg, condOp, condVal}) do
    if Process.whereis(condReg) == nil, do: Process.register(spawn(__MODULE__, :register_proc, [condReg, 0]), condReg)
    send(condReg, {:check, condOp, condVal, self()})
    receive do {:result, result} -> result end
  end

  defp parse_input(input) do
    input
    |> String.split("\n")
    |> Enum.map(&(String.split(&1, "if")))
    |> Enum.map(&parse_line/1)
  end

  defp parse_line([opString, condition]) do
    [register, op, val] = String.split(opString)
    [condRegister, condOperator, condVal] = String.split(condition)
    {String.to_atom(register), String.to_atom(op), String.to_integer(val), {String.to_atom(condRegister), String.to_atom(condOperator), String.to_integer(condVal)}}
  end
end
