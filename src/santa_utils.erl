-module(santa_utils).

-export([permutations/1, map_file/4]).


permutations(Elements) ->
	permutations([[El] || El <- Elements], Elements, []).
	


permutations([CurPer | RestPers], AllElems, FinishedPers) ->  
    case AllElems -- CurPer of 
        [] ->
            permutations(RestPers, AllElems, [CurPer | FinishedPers]);
        Remaining ->
            NewPers = [[NewElem | CurPer] || NewElem <- Remaining],
            permutations(NewPers ++ RestPers, AllElems, FinishedPers)
    end;
permutations([], _, FinishedPers) ->
    FinishedPers.

	

map_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).
 
for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                    for_each_line(Device, Proc, NewAccum)
    end.
	