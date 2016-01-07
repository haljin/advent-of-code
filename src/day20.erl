%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  @doc
%%  Once again - each house and each elf are a process. Elves simply send gifts
%%  to houses and once a house has enough gifts it notifies the result process.
%%
%%  This is quite a bit asynchronous, so the first result message might not be
%%  the actual answer. After some time result can be queried manually to get
%%  the answer, but in reality should probably check the answers when all the
%%  elves have quit.
%%  @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(day20).

-compile(export_all).

parse_data(File) ->
  {ok, Fd} = file:open(File, [read]),
  {ok, L} = file:read_line(Fd),
  list_to_integer(L -- "\n").


solve(Limit, Spawns) ->
  register(result, spawn(?MODULE, result_proc, [[]])),
  [register(house_name(N), spawn(?MODULE, house_proc, [N, 0, Limit, false])) || N <- lists:seq(1,Spawns)],
  [spawn(?MODULE, elf_proc, [N, N, Spawns]) || N <- lists:seq(1,Spawns)].

solve2(Limit, From, To) ->
  register(result, spawn(?MODULE, result_proc, [[]])),
  [register(house_name(N), spawn(?MODULE, house_proc, [N, 0, Limit, false])) || N <- lists:seq(From ,To)],
  [spawn(?MODULE, elf_proc2, [N, N, To, 50]) || N <- lists:seq(From,To)].

result_proc(Acc) ->
  receive
    {result, Res} ->
      result_proc([Res | Acc]);
    {get, Pid} ->
      case Acc of
        [] -> Pid ! no_result;
        _ -> Pid ! lists:min(Acc)
      end,
      result_proc(Acc);
    die ->
      ok
  end.

elf_proc(_N, CurrentHouse, MaxHouse) when CurrentHouse > MaxHouse ->
  ok;
elf_proc(N, CurrentHouse, MaxHouse) ->
  house_name(CurrentHouse) ! {gifts, 10 * N},
  elf_proc(N, CurrentHouse + N, MaxHouse).

elf_proc2(_N, CurrentHouse, MaxHouse, _Limit) when CurrentHouse > MaxHouse ->
  ok;
elf_proc2(_N, _CurrentHouse, _, 0) ->
  ok;
elf_proc2(N, CurrentHouse, MaxHouse, Limit) ->
  house_name(CurrentHouse) ! {gifts, 11 * N},
  elf_proc2(N, CurrentHouse + N, MaxHouse, Limit - 1).

house_proc(Number, Gifts, Limit, false) when Gifts >= Limit ->
  result ! {result, Number},
  house_proc(Number, Gifts, Limit, true);
house_proc(Number, Gifts, Limit, Done) ->
  receive
    {gifts, N} ->
      house_proc(Number, Gifts + N, Limit, Done);
    {state, Pid} ->
      Pid ! {Number, Gifts},
      house_proc(Number, Gifts, Limit, Done);
    die ->
      ok
  end.


house_name(N) ->
  list_to_atom("house_" ++ integer_to_list(N)).

