-module(day11).

-compile(export_all).

-record(state, {floor = 1,
                items = [{1, []},
                         {2, []},
                         {3, []},
                         {4, []}]}).

test_input() ->
  #state{items = [{1, [{microchip, hydrogen}, {microchip, lithium}]},
                  {2, [{generator, hydrogen}]},
                  {3, [{generator, lithium}]},
                  {4, []}]}.
input() ->
  #state{items = [{1, lists:sort([{generator, polonium},
                       {generator, thulium}, {microchip, thulium},
                       {generator, promethium}, {generator, ruthenium},
                       {microchip, ruthenium}, {generator, cobalt}, {microchip, cobalt}])},
                  {2, lists:sort([{microchip, polonium}, {microchip, promethium}])},
                  {3, []},
                  {4, []}]}.

test() ->
  solve([{0, test_input()}], [], undefined).

solve() ->
  solve([{0, input()}], [], undefined).

solve([], _, Min) ->
  Min;
solve([{Steps, _State} | Rest], Visited, Min) when Min =/= undefined,
                                                  Min < Steps ->
  solve(Rest, Visited, Min);
solve([{Steps, State} | Rest], Visited, Min) ->
  io:format("Now investigating ~p length of Visited: ~p ~n", [Steps, length(Visited)]),
  case {is_final(State), Min} of
    {true, undefined} ->
      io:format("Found new minimum ~p~n", [Steps]),
      solve(Rest, Visited, Steps);
    {true, Min} ->
      io:format("Found new minimum ~p~n", [Steps]),
      solve(Rest, Visited, min(Min, Steps));
    {false, _} ->
      PossibleMoves = possible_moves(State),
      NewStates = [{Steps + 1, apply_state_change(Move, State)} || Move <- PossibleMoves],
      FilteredNewStates = [{S, FilteredState} || {S, FilteredState} <- NewStates,
                           valid_state(FilteredState) andalso
                           not lists:any(fun({PrevStep, PrevState}) -> FilteredState == PrevState andalso PrevStep < Steps + 1 end, Visited)],
%%      JustStates = [JS || {_, JS} <- FilteredNewStates],
      solve(FilteredNewStates ++ Rest, FilteredNewStates ++ Visited, Min)
  end.

is_final(#state{items = Items}) ->
  lists:keyfind(3, 1, Items) =:= {3, []} andalso
    lists:keyfind(2, 1, Items) =:= {2, []} andalso
    lists:keyfind(1, 1, Items) =:= {1, []}.

valid_state(#state{items = Items}) ->
  lists:all(fun({_F, ItemsOnFloor}) ->
    Microchips = [M || {microchip, M} <- ItemsOnFloor],
    Generators = [G || {generator, G} <- ItemsOnFloor],
    Generators =:= [] orelse
    lists:all(fun(Element) -> lists:member(Element, Generators) end, Microchips)
            end, Items).

apply_state_change({Direction, ItemsToTake}, #state{floor = Floor, items = Items} = State) ->
  {_, ItemsOnFloor} = lists:keyfind(Floor, 1, Items),
  NewFloor = case Direction of
               up -> Floor + 1;
               down -> Floor - 1
             end,
  {_, ItemsAtNewFloor} = lists:keyfind(NewFloor, 1, Items),
  State#state{floor = NewFloor,
              items = lists:keyreplace(NewFloor, 1, lists:keyreplace(Floor, 1, Items, {Floor, lists:sort(ItemsOnFloor -- ItemsToTake)}),
                                       {NewFloor, lists:sort(ItemsAtNewFloor ++ ItemsToTake)})}.

possible_moves(State) ->
  [{Direction, ItemsToTake} || Direction <- possible_directions(State), ItemsToTake <- possible_items(State)].

possible_directions(#state{floor = 1}) ->
  [up];
possible_directions(#state{floor = 4}) ->
  [down];
possible_directions(_) ->
  [up, down].

possible_items(#state{floor = Floor, items = Items}) ->
  {_, ItemsOnFloor} = lists:keyfind(Floor, 1, Items),
  possible_items(ItemsOnFloor, ItemsOnFloor, []).

possible_items([], ItemsOnFloor, Acc) ->
  SingleItems = [[Item] || Item <- ItemsOnFloor],
  SingleItems ++ Acc;
possible_items([Item | Rest], ItemsOnFloor, Acc) ->
  NewCombos = [[Item, AnotherItem] || AnotherItem <- Rest],
  possible_items(Rest, ItemsOnFloor, NewCombos ++ Acc).