defmodule AdventOfCode2017.Day7 do
  @moduledoc """
  Day 6 solutions
  """

  @doc """
  Solves the first riddle of day 7.
  Examples:

      iex> AdventOfCode2017.Day7.solve("pbga (66)
      ...> xhth (57)
      ...> ebii (61)
      ...> havc (66)
      ...> ktlj (57)
      ...> fwft (72) -> ktlj, cntj, xhth
      ...> qoyq (66)
      ...> padx (45) -> pbga, havc, qoyq
      ...> tknk (41) -> ugml, padx, fwft
      ...> jptl (61)
      ...> ugml (68) -> gyxo, ebii, jptl
      ...> gyxo (61)
      ...> cntj (57)")
      :tknk

  """
  def solve(input) do
    input
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> (fn(procs) -> {find_root(procs), procs} end).()
    |> (fn({root, procs}) -> Enum.map(procs, fn ({name, _, _parentName}) -> send(name, :die) end); root end).()
  end

  @doc """
  Solves the second riddle of day 7. 
  Examples:

      iex> AdventOfCode2017.Day7.solve2("pbga (66)
      ...> xhth (57)
      ...> ebii (61)
      ...> havc (66)
      ...> ktlj (57)
      ...> fwft (72) -> ktlj, cntj, xhth
      ...> qoyq (66)
      ...> padx (45) -> pbga, havc, qoyq
      ...> tknk (41) -> ugml, padx, fwft
      ...> jptl (61)
      ...> ugml (68) -> gyxo, ebii, jptl
      ...> gyxo (61)
      ...> cntj (57)")
      60

  """
  def solve2(input) do
    input
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> find_root
    |> send({:get_weight, self()})
    receive do {:unbalanced, _name, targetWeight} -> targetWeight end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  def program_proc(name, weight, children, parent) do
    receive do
      {:get_weight, pid} ->
        Enum.map(children, fn child -> send(child, {:get_weight, self()}); receive do res -> {child, res} end end)
        |> process_responses([], weight)
        |> (fn res -> send(pid, res) end).()
      {:new_parent, newParent} ->
        program_proc(name, weight, children, newParent)
      {:tell_children, pid} ->
        Enum.each(children, &(send(&1, {:new_parent, name})))
        send(pid, {:ok, name})
        program_proc(name, weight, children, parent)
      {:get_parent, pid} ->
        send(pid, {:parent, parent})
        program_proc(name, weight, children, parent)
      :die ->
        :ok
    end    
  end  

  defp find_root(tree) do
    tree
    |> Enum.map(fn ({name, weight, children}) -> Process.register(spawn(__MODULE__, :program_proc, [name, weight, children, nil]), name); name end)
    |> Enum.map(fn (name) -> send(name, {:tell_children, self()}); receive do {:ok, name} -> name end end)
    |> Enum.map(fn (name) -> send(name, {:get_parent, self()}); receive do {:parent, parentName} -> {name, parentName} end end)
    |> Enum.find(&(elem(&1, 1) == nil))
    |> elem(0)
  end

  defp process_responses([{_, {:unbalanced, name, targetWeight}} | _rest], _weights, _myWeight) do
    {:unbalanced, name, targetWeight}
  end
  defp process_responses([{child, {:weight, hisWeight, childreWeight}} | rest], weights, myWeight) do
    process_responses(rest, [{child, hisWeight, childreWeight} | weights], myWeight)
  end
  defp process_responses([], weights, myWeight) do
    grouped = 
    Enum.group_by(weights, &(elem(&1, 1) + elem(&1, 2)), &(elem(&1, 0)))
    |> Map.to_list
    case length(grouped) do
      0 -> 
        {:weight, myWeight, 0}
      1 -> 
        {childrenWeight, children} = hd(grouped)
        {:weight, myWeight, childrenWeight * length(children)}
      _ ->
        {{wrongWeight, [wrongName]}, {correctWeight, _}} = Enum.min_max_by(grouped, &(length(elem(&1, 1))))
        {wrongName, hisOwnWeight, _} = Enum.find(weights, &(elem(&1, 0) == wrongName))
        {:unbalanced, wrongName, correctWeight - wrongWeight + hisOwnWeight}
    end
    
  end


  defp parse_line(line) do
    line
    |> String.split("->")
    |> (fn ([proc | children]) -> 
        [procName, weightString] = String.split(proc)
        {
          String.to_atom(procName),
          String.replace(weightString, ~r"\)|\(", "") |> String.to_integer,
          case children do
            [] -> []
            [children] -> String.split(children, ",") |> Enum.map(&String.trim/1) |> Enum.map(&String.to_atom/1) 
          end
        }    
      end).()
  end

end
