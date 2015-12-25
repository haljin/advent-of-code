-module(day18).

-compile(export_all).

parse_data(File) ->
  santa_utils:map_file(File,
                       fun(Line, Acc) ->
                          [Line -- "\n" | Acc]
                        end, [read], []).


solve(LightStates, Steps) ->
  YSize = length(LightStates),
  XSize = length(hd(LightStates)),
  Lights = [{X, Y} || X <- lists:seq(1, XSize), Y <- lists:seq(1, YSize)],
  LightNames = [light_name(X, Y) || X <- lists:seq(1, XSize), Y <- lists:seq(1, YSize)],
  make_lights(LightStates, 1, 1, Lights),
  spawn(?MODULE, sync_proc, [Steps, LightNames]).

solve2(LightStates, Steps) ->
  YSize = length(LightStates),
  XSize = length(hd(LightStates)),
  Lights = [{X, Y} || X <- lists:seq(1, XSize), Y <- lists:seq(1, YSize)],
  LightNames = [light_name(X, Y) || X <- lists:seq(1, XSize), Y <- lists:seq(1, YSize)],
  make_lights(LightStates, 1, 1, Lights),
  light_name(1,1) ! stuck_on,
  light_name(1,YSize) ! stuck_on,
  light_name(XSize,1) ! stuck_on,
  light_name(XSize,YSize) ! stuck_on,
  spawn(?MODULE, sync_proc, [Steps, LightNames]).



make_lights([[SChar| XRow]|YRows], CX, CY, AllLights) ->
  Neighbours = [light_name(X, Y) || {X, Y} <- AllLights, (X >= CX - 1 andalso X =< CX + 1) andalso
                                                         (Y >= CY - 1 andalso Y =< CY + 1) andalso
                                                         (X =/= CX orelse Y =/= CY)],
  State = case SChar of
            $# -> on;
            $. -> off
          end,
  Pid = spawn(?MODULE, light_proc, [Neighbours, State]),
  register(light_name(CX, CY), Pid),
  make_lights([XRow | YRows], CX + 1, CY, AllLights);
make_lights([[]|YRows], _CX, CY, AllLights) ->
  make_lights(YRows, 1, CY + 1, AllLights);
make_lights([], _CX, _CY, _AllLights) ->
  ok.


light_proc(Neighbours, State) ->
  receive
    {get_state, Pid} ->
      Pid ! {state, State};
    stuck_on ->
      light_stuck(Neighbours);
    {sync, Pid} ->
      [N ! {neighbour_state, State} || N <- Neighbours],
      NextState = check_neighbours(length(Neighbours), State, []),
      Pid ! {state, State},
      light_proc(Neighbours, NextState)
  end.

light_stuck(Neighbours) ->
  receive
    {get_state, Pid} ->
      Pid ! {state, on};
    permament_on ->
      light_stuck(Neighbours);
    {sync, Pid} ->
      [N ! {neighbour_state, on} || N <- Neighbours],
      check_neighbours(length(Neighbours), on, []),
      Pid ! {state, on},
      light_stuck(Neighbours)
  end.

check_neighbours(N, State, Results) when N > 0 ->
  receive
    {neighbour_state, S} ->
      check_neighbours(N - 1, State, [S | Results])
  end;
check_neighbours(0, on, Results) ->
    case length([on || on <- Results]) of
      2 -> on;
      3 -> on;
      _ -> off
    end;
check_neighbours(0, off, Results) ->
  case length([on || on <- Results]) of
    3 -> on;
    _ -> off
  end.

sync_proc(0, Lights) ->
  [Light ! {get_state, self()} || Light <- Lights],
  Results = receive_sync(length(Lights), []),
  io:format("~p~n", [length([R || R <- Results, R =:= on])]);
sync_proc(Steps, Lights) ->
  [Light ! {sync, self()} || Light <- Lights],
  receive_sync(length(Lights), []),
  sync_proc(Steps - 1,Lights).

receive_sync(N, Acc) when N > 0 ->
  receive
    {state, State} -> receive_sync(N -1 , [State|Acc])
  end ;
receive_sync(0, Acc) ->
  Acc.

light_name(X, Y) ->
  list_to_atom(lists:flatten(io_lib:format("~p_~p", [X,Y]))).


