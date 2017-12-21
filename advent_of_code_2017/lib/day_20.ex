defmodule AdventOfCode2017.Day20 do
  @moduledoc """
  Day 20 solutions
  """

  @doc """
  Solves the first riddle of day 20.
  Examples:

      iex> AdventOfCode2017.Day20.solve("p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
      ...> p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")
      0
  """
  def solve(input) do
    input
    |> parse
    |> tick_many(1000)
    |> Enum.with_index
    |> Enum.min_by(fn {data, _pos} -> {x, y, z} = data.p; abs(x) + abs(y) + abs(z) end)
    |> elem(1)
  end

  @doc """
  Solves the second riddle of day 20. 
  Examples:
  
      iex> AdventOfCode2017.Day20.solve2("p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
      ...> p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
      ...> p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
      ...> p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>")
      1

  """
  def solve2(input) do
    input
    |> parse
    |> remove_collisions([])
    |> tick_many_colisions(100)
    |> length
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------
  defp parse(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&(String.split(&1, ", ")))
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(["p=" <> p, "v=" <> v, "a=" <> a]) do
    %{p: parse_coords(p), v: parse_coords(v), a: parse_coords(a)}
  end

  defp parse_coords(coords) do
    String.replace(coords, ~r"<|>", "")
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple
  end
  
  defp tick_many(particles, 0), do: particles
  defp tick_many(particles, n) do
    Enum.map(particles, &tick/1)
    |> tick_many(n- 1)
  end

  defp tick_many_colisions(particles, 0), do: particles
  defp tick_many_colisions(particles, n) do
    Enum.map(particles, &tick/1)
    |> remove_collisions([])
    |> tick_many_colisions(n- 1)
  end

  defp tick(%{p: {px, py, pz}, v: {vx, vy, vz}, a: {ax, ay, az}}) do
    %{p: {px + vx + ax, py + vy + ay, pz + vz + az},
      v: {vx + ax, vy + ay, vz + az},
      a: {ax, ay, az}}
  end

  defp remove_collisions([], safe), do: safe
  defp remove_collisions([particle | rest], safe) do
    case Enum.any?(rest, fn par -> par.p == particle.p end) do
      true -> 
        Enum.filter(rest, fn par -> par.p != particle.p end)
        |> remove_collisions(safe)
      false -> 
        remove_collisions(rest, [particle | safe])
    end
  end
end
