defmodule AdventOfCode2017.AdventMacros do

    defmacro adventdoctest do
        days = 
        File.ls("lib") 
        |> elem(1) 
        |> Enum.filter(fn "day" <> _ -> true; _ -> false end) 
        |> Enum.map(&(String.trim(&1, ".ex"))) 
        |> Enum.map(&Macro.camelize/1)
        |> Enum.map(&String.to_atom/1)
        for day <- days do
            dayModName = Module.concat([AdventOfCode2017, day])
            quote do 
                alias unquote(dayModName)
                doctest unquote(dayModName) 
            end
        end
    end
end