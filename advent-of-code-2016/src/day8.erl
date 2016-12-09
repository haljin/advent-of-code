-module(day8).

-compile(export_all).
-define(SIZE_X, 50).
-define(SIZE_Y, 6).

parse_data(File) ->
  santa_utils:map_file(     File, fun(X, Acc) ->
    Acc ++ [X -- "\n"] end, [read], []).

solve() ->
  Data = parse_data("../priv/day8input.txt"),
  [spawn_pixel(X, Y) || X <- lists:seq(1, ?SIZE_X), Y <- lists:seq(1, ?SIZE_Y)],
  solve(Data).

solve([]) ->
  State = get_state(),
  Count = length([C || on = C <- lists:flatten(State)]),
  print_state(State),
  kill_all(),
  Count;
solve(["rect " ++ Data | Rest]) ->
  [SizeX, SizeY] = [list_to_integer(N) || N <- string:tokens(Data, "x")],
  Sent = [pname(X, Y) ! {rect, self()} || X <- lists:seq(1, SizeX), Y <- lists:seq(1, SizeY)],
  get_answers(length(Sent)),
  solve(Rest);
solve(["rotate row y=" ++ Data | Rest]) ->
  [RowY, Rot] = [list_to_integer(N) || N <- string:tokens(Data, " by")],
  pname(1, RowY + 1) ! {rotate_hor, self(), Rot},
  get_answers(1),
  solve(Rest);
solve(["rotate column x=" ++ Data | Rest]) ->
  [ColX, Rot] = [list_to_integer(N) || N <- string:tokens(Data, " by")],
  pname(ColX + 1, 1) ! {rotate_vert, self(), Rot},
  get_answers(1),
  solve(Rest).



spawn_pixel(X, Y) ->
  Pid = spawn(?MODULE, pixel_proc, [X, Y, off]),
  register(pname(X, Y), Pid).

pname(X, Y) ->
  list_to_atom("pixel" ++ integer_to_list(X) ++ "_" ++ integer_to_list(Y)).


pixel_proc(MyX, MyY, State) ->
  receive
    {rect, From} ->
      From ! {done, self()},
      pixel_proc(MyX, MyY, on);
    {rotate_vert, From, N} ->
      send_vert_rot(MyX, MyY, N, State),
      rotation(MyX, MyY, From, State);
    {rotate_hor, From, N} ->
      send_hor_rot(MyX, MyY, N, State),
      rotation(MyX, MyY, From, State);
    {rot_v, NewState, N} ->
      send_vert_rot(MyX, MyY, N, State),
      pixel_proc(MyX, MyY, NewState);
    {rot_h, NewState, N} ->
      send_hor_rot(MyX, MyY, N, State),
      pixel_proc(MyX, MyY, NewState);
    {get_state, From} ->
      From ! {state, State},
      pixel_proc(MyX, MyY, State);
    die ->
      ok
  end.

rotation(MyX, MyY, From, _State) ->
  receive
    {rot_v, NewState, 1} ->
      From ! {done, self()},
      pixel_proc(MyX, MyY, NewState);
    {rot_v, NewState, N} ->
      send_vert_rot(MyX, MyY, N - 1, NewState),
      rotation(MyX, MyY, From, NewState);
    {rot_h, NewState, 1} ->
      From ! {done, self()},
      pixel_proc(MyX, MyY, NewState);
    {rot_h, NewState, N} ->
      send_hor_rot(MyX, MyY, N - 1, NewState),
      rotation(MyX, MyY, From, NewState)
  end.


send_hor_rot(MyX, MyY, N, State) when MyX =:= ?SIZE_X ->
  pname(1, MyY) ! {rot_h, State, N};
send_hor_rot(MyX, MyY, N, State) ->
  pname(MyX + 1, MyY) ! {rot_h, State, N}.


send_vert_rot(MyX, MyY, N, State) when MyY =:= ?SIZE_Y ->
  pname(MyX, 1) ! {rot_v, State, N};
send_vert_rot(MyX, MyY, N, State) ->
  pname(MyX, MyY + 1) ! {rot_v, State, N}.

get_answers(N) when N > 0 ->
  receive
    {done, _} -> get_answers(N - 1)
  end;
get_answers(0) ->
  ok.

get_state(X, Y) ->
  pname(X, Y) ! {get_state, self()},
  receive
    {state, State} -> State
  end.


kill_all() ->
  [pname(X, Y) ! die || X <- lists:seq(1, ?SIZE_X), Y <- lists:seq(1, ?SIZE_Y)].

get_state() ->
  [[get_state(X, Y) || X <- lists:seq(1, ?SIZE_X)] || Y <- lists:seq(1, ?SIZE_Y)].

print_state([Row | Rest]) ->
  SignRow = [to_sign(X) || X <- Row],
  io:format("~p", [SignRow]),
  io:format("~n"),
  print_state(Rest);
print_state([]) ->
  ok.

to_sign(on)  -> $#;
to_sign(off) -> $..