%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  @doc
%%  In order to handle a large two-dimensional array, instead of playing with
%%  lists of lists, this example spawns a process for each Christmas light!
%%
%%  Each process PID is registered into the local registry and list comprehensions
%%  are used to construct the process name. While it is quite slow to do this
%%  with a million processes it seems to be much faster than doing complicated
%%  lookups on an ETS table as was in the previous solution.
%%
%%  Local registry + comprehensions gain almost 4 times increase in performance.
%%  @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(day6).

-compile(export_all).


parse_data(Path) ->
  santa_utils:map_file(Path, fun(X, Acc) ->
    Entry = case string:tokens(string:strip(X, both, $\n), " ,") of
              ["turn", "on", X1, Y1, "through", X2, Y2] ->
                {on, {list_to_integer(X1),list_to_integer(Y1)}, {list_to_integer(X2), list_to_integer(Y2)}};
              ["turn", "off", X1, Y1, "through", X2, Y2] ->
                {off, {list_to_integer(X1),list_to_integer(Y1)}, {list_to_integer(X2), list_to_integer(Y2)}};
              ["toggle", X1, Y1, "through", X2, Y2] ->
                {toggle, {list_to_integer(X1),list_to_integer(Y1)}, {list_to_integer(X2), list_to_integer(Y2)}}
            end,
    Acc ++ [Entry]
                              end, [read], []).

solve(ListofStrings) ->
%%  Tab = ets:new(pid_table, []),
  [[begin
      Pid = spawn(?MODULE, light_process, [false]),
      register(light_name(X,Y), Pid)
%%      ets:insert(Tab, {{X,Y}, Pid})
    end || Y <- lists:seq(0,999)] ||  X <- lists:seq(0,999)],
  io:format("Starting at ~p~n", [calendar:now_to_local_time(erlang:timestamp())]),
  process_day6(ListofStrings).

process_day6([{on, {X1, Y1}, {X2, Y2}}| T]) ->
  [LightPid ! on || LightPid <- get_light_range({X1, Y1}, {X2, Y2})],
  process_day6(T);
process_day6([{off, {X1, Y1}, {X2, Y2}}| T]) ->
  [LightPid ! off || LightPid <- get_light_range({X1, Y1}, {X2, Y2})],
  process_day6(T);
process_day6([{toggle, {X1, Y1}, {X2, Y2}}| T]) ->
  [LightPid ! toggle || LightPid <- get_light_range({X1, Y1}, {X2, Y2})],
  process_day6(T);
process_day6([]) ->
  io:format("Done, collecting results ~p~n", [calendar:now_to_local_time(erlang:timestamp())]),
  Result = [begin
              LightPid ! {get_state, self()},
              receive
                true -> 1;
                false -> 0;
                Val -> Val
              end
            end || LightPid <- get_light_range({0,0}, {999,999})],
%%  ets:select(Ets, [{{{'$1', '$2'}, '$3'}, [], ['$3']}])],
%%  ets:delete(Ets),
  io:format("Summing now ~p~n", [calendar:now_to_local_time(erlang:timestamp())]),
  lists:sum(Result).

get_light_range({X1,Y1},{X2,Y2}) when X1 =< X2,
                                      Y1 =< Y2 ->
  [light_name(X, Y) ||  X <- lists:seq(X1,X2), Y <- lists:seq(Y1,Y2)].

get_light_range({X1,Y1},{X2,Y2}, Tab) when X1 =< X2,
                                           Y1 =< Y2 ->
  ets:select(Tab, [{{{'$1', '$2'}, '$3'}, [{'>=', '$1', X1}, {'=<', '$1', X2},
                                           {'>=', '$2', Y1}, {'=<', '$2', Y2}], ['$3']}]).

light_name(X,Y) ->
  list_to_atom("light_" ++ integer_to_list(X) ++ "_" ++ integer_to_list(Y)).

solve2(ListofStrings) ->
  Tab = ets:new(pid_table, []),
  [[begin
      Pid = spawn(?MODULE, bightness_light_process, [0]),
      ets:insert(Tab, {{X,Y}, Pid})
    end || Y <- lists:seq(0,999)] ||  X <- lists:seq(0,999)],
  io:format("Starting at ~p~n", [calendar:now_to_local_time(erlang:timestamp())]),
  process_day6(ListofStrings).

light_process(LightState) ->
  receive
    off ->
      light_process(false);
    on ->
      light_process(true);
    toggle ->
      light_process(not LightState);
    {get_state, Pid} ->
      Pid ! LightState
  end.

bightness_light_process(Brigthness) ->
  receive
    off when Brigthness =:= 0 ->
      bightness_light_process(0);
    off ->
      bightness_light_process(Brigthness - 1);
    on ->
      bightness_light_process(Brigthness + 1);
    toggle ->
      bightness_light_process(Brigthness + 2);
    {get_state, Pid} ->
      Pid ! Brigthness
  end.