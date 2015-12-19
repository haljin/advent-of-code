-module(day14).

-compile(export_all).

parse_data(File) ->
	santa_utils:map_file(File, fun(Line, Acc) ->
									[Name, "can", "fly", Speed, _, _, Time, _, _, _, _, _, _, RestTime, _] = string:tokens(Line -- ".\n", " "),
									
									[{list_to_atom(Name), list_to_integer(Speed), list_to_integer(Time), list_to_integer(RestTime)}|Acc]
								end, [read], []).
								

solve(Reindeers, TotalTime) ->
	[spawn(?MODULE, reindeer_flying, [Reindeer, self(), TotalTime, Speed, MaxFlight, 0, Rest, 0])
		|| {Reindeer, Speed, MaxFlight, Rest} <- Reindeers],
	get_distance(length(Reindeers), []).

get_distance(N, Acc) when N > 0 ->
	receive
		{flown, Name, Distance} ->
			get_distance(N-1 , [{Name, Distance} | Acc])
	end;
get_distance(0, Acc) ->
	Acc.
	
solve2(Reindeers, TotalTime) ->
	[spawn(?MODULE, reindeer_flying2, [Reindeer, self(), TotalTime, Speed, MaxFlight, 0, Rest, 0])
		|| {Reindeer, Speed, MaxFlight, Rest} <- Reindeers],
	Results = get_distance2(length(Reindeers) * (TotalTime + 1), #{}),
	ResultsMap = maps:map(fun(_, Val) -> get_winners(lists:keysort(2, Val), 0, []) end, Results),
	count_points(TotalTime, maps:to_list(ResultsMap), #{}).
	
count_points(TotalTime, [{TotalTime, _} | Rest], Map) ->
	count_points(TotalTime, Rest, Map);
count_points(TotalTime, [{_Timestamp, Winners} | Rest], Map) when is_list(Winners) ->
	NewMap = lists:foldl(fun(Reindeer, Acc) ->
							case maps:find(Reindeer, Acc) of
								error ->
									maps:put(Reindeer, 1, Acc);
								{ok, Score} ->
									maps:put(Reindeer, Score + 1, Acc)
							end
						end, Map, Winners),
	count_points(TotalTime, Rest, NewMap);
count_points(TotalTime, [{_Timestamp, Winner} | Rest], Map) ->
	NewMap = case maps:find(Winner, Map) of
					error ->
						maps:put(Winner, 1, Map);
					{ok, Score} ->
						maps:put(Winner, Score + 1, Map)
			 end,
	count_points(TotalTime, Rest, NewMap);
count_points(_, [], Map) ->
	Map.
	
get_winners([{Reindeer, Score} | T], WinScore, _Winners) when Score > WinScore ->
	get_winners(T, Score, [Reindeer]);
get_winners([{Reindeer, WinScore} | T], WinScore, Winners) ->
	get_winners(T, WinScore, [Reindeer | Winners]);
get_winners([_Loser | T], WinScore, Winners) ->
	get_winners(T, WinScore, Winners);
get_winners([], _WinScore, Winners) ->
	Winners.	
	
get_distance2(N, Acc) when N > 0 ->
	receive
		{flown, Name, Timestamp, Distance} ->
			TimeResults = case maps:find(Timestamp, Acc) of
											error -> [];
											{ok, Res} -> Res
						   end,
			get_distance2(N-1 , maps:put(Timestamp, [{Name, Distance} | TimeResults], Acc))
	end;
get_distance2(0, Acc) ->
	Acc.	

reindeer_flying(Name, CtrlPid, 0, _Speed, _MaxFlightTime, _CurrentFlightTime, _RestTime, Distance) ->
	CtrlPid ! {flown, Name, Distance};
reindeer_flying(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, CurrentFlightTime, RestTime, Distance) when CurrentFlightTime < MaxFlightTime ->
	reindeer_flying(Name, CtrlPid, TimeLeft - 1, Speed, MaxFlightTime, CurrentFlightTime + 1, RestTime, Distance + Speed);	
reindeer_flying(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, MaxFlightTime, RestTime, Distance) ->
	reindeer_resting(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, 0, RestTime, Distance).
		
reindeer_resting(Name, CtrlPid, 0, _Speed, _MaxFlightTime, _CurrentRestTime, _RestTime, Distance) ->
	CtrlPid ! {flown, Name, Distance};
reindeer_resting(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, CurrentRestTime, RestTime, Distance) when CurrentRestTime < RestTime ->
	reindeer_resting(Name, CtrlPid, TimeLeft - 1, Speed, MaxFlightTime, CurrentRestTime + 1, RestTime, Distance);	
reindeer_resting(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, RestTime, RestTime, Distance) ->
	reindeer_flying(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, 0, RestTime, Distance).
	
	
reindeer_flying2(Name, CtrlPid, 0, _Speed, _MaxFlightTime, _CurrentFlightTime, _RestTime, Distance) ->
	CtrlPid ! {flown, Name, 0, Distance},
	ok;
reindeer_flying2(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, CurrentFlightTime, RestTime, Distance) when CurrentFlightTime < MaxFlightTime ->
	CtrlPid ! {flown, Name, TimeLeft, Distance},
	reindeer_flying2(Name, CtrlPid, TimeLeft - 1, Speed, MaxFlightTime, CurrentFlightTime + 1, RestTime, Distance + Speed);	
reindeer_flying2(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, MaxFlightTime, RestTime, Distance) ->
	reindeer_resting2(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, 0, RestTime, Distance).
		
reindeer_resting2(Name, CtrlPid, 0, _Speed, _MaxFlightTime, _CurrentRestTime, _RestTime, Distance) ->
	CtrlPid ! {flown, Name, 0, Distance},
	ok;
reindeer_resting2(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, CurrentRestTime, RestTime, Distance) when CurrentRestTime < RestTime ->
	CtrlPid ! {flown, Name, TimeLeft, Distance},
	reindeer_resting2(Name, CtrlPid, TimeLeft - 1, Speed, MaxFlightTime, CurrentRestTime + 1, RestTime, Distance);	
reindeer_resting2(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, RestTime, RestTime, Distance) ->
	reindeer_flying2(Name, CtrlPid, TimeLeft, Speed, MaxFlightTime, 0, RestTime, Distance).