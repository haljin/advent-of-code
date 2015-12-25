-module(day13).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File, fun(Line, Acc) ->
    [Name, "would", LoseGain, Happiness, _, _, _, _, _, _, OtherName] = string:tokens(Line -- ".\n", " "),
    RealHappiness = case LoseGain of
                      "lose" ->
                        -list_to_integer(Happiness);
                      "gain" ->
                        list_to_integer(Happiness)
                    end,
    [{list_to_atom(Name), RealHappiness, list_to_atom(OtherName)} | Acc]
  end,                 [read], []).


solve(Feelings) ->
  Names = lists:usort([Name || {Name, _, _} <- Feelings]),
  create_people(Names, Feelings),
  Permutations = santa_utils:permutations(Names),
  Feels = solve(Permutations, Feelings, []),
  [Guest ! die || Guest <- Names],
  lists:max(Feels).

solve([Seating | Rest], Feelings, Acc) ->
  seat_guests(hd(Seating), Seating),
  [Guest ! {feelings, self()} || Guest <- Seating],
  solve(Rest, Feelings, [count_feels(length(Seating), 0) | Acc]);
solve([], _, Acc) ->
  Acc.

solve2(Feelings) ->
  Names = lists:usort([Name || {Name, _, _} <- Feelings]),
  RealFeelings = [{'Me', 0, Name} || Name <- Names] ++ [{Name, 0, 'Me'} || Name <- Names] ++ Feelings,
  solve(RealFeelings).

count_feels(N, Acc) when N > 0 ->
  receive
    {feelings, Feels} ->
      count_feels(N - 1, Acc + Feels)
  end;
count_feels(0, Acc) ->
  Acc.

seat_guests(First, [Person, RightPerson | Rest]) ->
  Person ! {right, RightPerson},
  RightPerson ! {left, Person},
  seat_guests(First, [RightPerson | Rest]);
seat_guests(First, [Person]) ->
  Person ! {right, First},
  First ! {left, Person}.

create_people(Names, Feelings) ->
  [begin
     PersonsFeelings = [Feel || {N, _, _} = Feel <- Feelings, N =:= Name],
     register(Name, spawn(?MODULE, person_proc, [undefined, undefined, PersonsFeelings]))
   end || Name <- Names].

person_proc(LeftNeighbour, RightNeighbour, Feelings) ->
  receive
    {right, Name} ->
      person_proc(LeftNeighbour, Name, Feelings);
    {left, Name} ->
      person_proc(Name, RightNeighbour, Feelings);
    {feelings, Pid} ->
      Feels = [H || {_, H, Neighbour} <- Feelings,
        (Neighbour =:= LeftNeighbour) orelse (Neighbour =:= RightNeighbour)],
      Pid ! {feelings, lists:sum(Feels)},
      person_proc(undefined, undefined, Feelings);
    die ->
      ok
  end.