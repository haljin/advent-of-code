-module(day13).

-compile(export_all).


input() -> 1358.

solve() ->
  [register(proc_name(X, Y), spawn(?MODULE, grid_proc, [X, Y, input(), self()])) || X <- lists:seq(0, 45), Y <- lists:seq(0, 45)],
  area_1_1 ! {flow, 0},
  receive
    {result, Result} ->
      Queries = [proc_name(X, Y) ! {get, self()} || X <- lists:seq(0, 45), Y <- lists:seq(0, 45)],
      Under50 = collect_results(length(Queries), 0),
      [proc_name(X, Y) ! die || X <- lists:seq(0, 45), Y <- lists:seq(0, 45)],
      {Result, Under50}
  end.

collect_results(0, Acc) ->
  Acc;
collect_results(N, Acc) ->
%%  io:format("Collecting ~pth result~n", [N]),
  receive
    {value, M} when M =< 50 ->
      collect_results(N -1, Acc + 1);
    Msg ->
%%      io:format("received ~p ~n", [Msg]),
      collect_results(N -1, Acc)
  end.

grid_proc(X, Y, MagicNumber, ResPid) ->
  Val = X*X + 3*X + 2*X*Y + Y + Y*Y + MagicNumber,
  Ones = [1 || <<1:1>> <= binary:encode_unsigned(Val)],
  case length(Ones) rem 2 of
    1 -> wall_proc();
    0 -> area_proc(X, Y, undefined, ResPid)
  end.

wall_proc() ->
  receive
    die -> ok;
    {get, ResPid} ->
      ResPid ! {value, undefined},
      wall_proc();
    _ -> wall_proc()
  end.

area_proc(31, 39, undefined, ResPid) ->
  receive
    die -> ok;
    {flow, N} ->
      ResPid ! {result, N},
      flow_to_neighbours(31, 39, N),
      area_proc(31, 39, N, ResPid)
  end;
area_proc(X, Y, undefined, ResPid) ->
  receive
    die -> ok;
    {get, ResPid} ->
      ResPid ! {value, undefined},
      area_proc(X, Y, undefined, ResPid);
    {flow, N} ->
      flow_to_neighbours(X, Y, N),
      area_proc(X, Y, N, ResPid)
  end;
area_proc(X, Y, N, ResPid) ->
  receive
    die -> ok;
    {get, Pid} ->
      Pid ! {value, N},
      area_proc(X, Y, N, ResPid);
    {flow, _} ->
      area_proc(X, Y, N, ResPid)
  end.

flow_to_neighbours(X, Y, N) ->
  Neighbours = [proc_name(NX,NY) || {NX, NY} <- [{X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}]],
  [case whereis(Neighbour) of
     undefined -> skip;
     Pid -> Pid ! {flow, N+1}
   end || Neighbour <- Neighbours].


proc_name(X, Y) ->
  list_to_atom("area_" ++ integer_to_list(X) ++ "_" ++ integer_to_list(Y)).