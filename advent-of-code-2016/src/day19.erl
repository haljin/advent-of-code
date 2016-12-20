-module(day19).

-compile(export_all).

input() -> 3014603.

test_input() -> 5.

solve() ->
  {ElfNo1, _AllElves} = spawn_elves(input(), [], input()),
  register(host, self()),
  ElfNo1 ! your_turn,
  receive
    {i_win, ElfId} -> ElfId
  end.

solve2() ->
  {ElfNo1, _AllElves} = spawn_elves2(input(), [], input()),
  register(host, self()),
  ElfNo1 ! your_turn,
  receive
    {i_win, ElfId} -> ElfId
  end.

test2() ->
  {ElfNo1, _AllElves} = spawn_elves2(test_input(), [], test_input()),
  register(host, self()),
  ElfNo1 ! {your_turn, test_input()},
  receive
    {i_win, ElfId} -> ElfId
  end.

spawn_elves(N, [], Max) when N > 0 ->
  NewElf = spawn(?MODULE, elf_proc, [N, undefined, undefined, 1, Max]),
  spawn_elves(N - 1, [NewElf], Max);
spawn_elves(N, [PrevElf | _] = Acc, Max) when N > 0 ->
  NewElf = spawn(?MODULE, elf_proc, [N, PrevElf, undefined, 1, Max]),
  spawn_elves(N - 1, [NewElf | Acc], Max);
spawn_elves(0, Acc, _Max) ->
  build_chain(hd(Acc), Acc, []).

spawn_elves2(N, [], Max) when N > 0 ->
  NewElf = spawn(?MODULE, elf_proc2, [N, undefined, undefined, 1, Max]),
  spawn_elves2(N - 1, [NewElf], Max);
spawn_elves2(N, [PrevElf | _] = Acc, Max) when N > 0 ->
  NewElf = spawn(?MODULE, elf_proc2, [N, PrevElf, undefined, 1, Max]),
  spawn_elves2(N - 1, [NewElf | Acc], Max);
spawn_elves2(0, Acc, _Max) ->
  build_chain(hd(Acc), Acc, []).

build_chain(LastElf, [Elf], [PrevElf | _] = Acc) ->
  Elf ! {right, PrevElf},
  Elf ! {left, LastElf},
  LastElf ! {right, Elf},
  {LastElf, [Elf | Acc]};
build_chain(LastElf, [Elf | Rest], []) ->
  build_chain(LastElf, Rest, [Elf]);
build_chain(LastElf, [Elf | Rest], [PrevElf | _] = Acc) ->
  Elf ! {right, PrevElf},
  build_chain(LastElf, Rest, [Elf | Acc]).


elf_proc(ElfId, _, _, Gifts, Gifts) ->
  host ! {i_win, ElfId};
elf_proc(ElfId, LeftElf, RightElf, Gifts, MaxGifts) ->
%%  io:format("Elf ~p: I'm at ~p and my neighbours are left: ~p right:~p~n", [ElfId, self(), LeftElf, RightElf]),
  receive
    {right, NewRightElf} ->
%%      io:format("Elf ~p: I'm at ~p and my neighbours are left: ~p right:~p~n", [ElfId, self(), LeftElf, NewRightElf]),
      elf_proc(ElfId, LeftElf, NewRightElf, Gifts, MaxGifts);
    {left, NewLeftElf} ->
%%      io:format("Elf ~p: I'm at ~p and my neighbours are left: ~p right:~p~n", [ElfId, self(), NewLeftElf, RightElf]),
      elf_proc(ElfId, NewLeftElf, RightElf, Gifts, MaxGifts);
    your_turn ->
%%      io:format("Elf ~p: I am gonna kill someone! :)~n", [ElfId]),
      LeftElf ! get_gifts,
      receive
        {gifts, MoreGifts, NewLeftElf} ->
          NewLeftElf ! your_turn,
          elf_proc(ElfId, NewLeftElf, RightElf, Gifts + MoreGifts, MaxGifts)
      end;
    get_gifts ->
%%      io:format("Elf ~p: I am dead :(~n", [ElfId]),
      LeftElf ! {right, RightElf},
      RightElf ! {gifts, Gifts, LeftElf}
  end.

elf_proc2(ElfId, _, _, Gifts, Gifts) ->
  host ! {i_win, ElfId};
elf_proc2(ElfId, LeftElf, RightElf, Gifts, MaxGifts) ->
%%  io:format("Elf ~p: I'm at ~p and my neighbours are left: ~p right:~p~n", [ElfId, self(), LeftElf, RightElf]),
  receive
    {right, NewRightElf} ->
%%      io:format("Elf ~p: I'm at ~p and my neighbours are left: ~p right:~p~n", [ElfId, self(), LeftElf, NewRightElf]),
      elf_proc2(ElfId, LeftElf, NewRightElf, Gifts, MaxGifts);
    {left, NewLeftElf} ->
%%      io:format("Elf ~p: I'm at ~p and my neighbours are left: ~p right:~p~n", [ElfId, self(), NewLeftElf, RightElf]),
      elf_proc2(ElfId, NewLeftElf, RightElf, Gifts, MaxGifts);
    {your_turn, TotalElves} ->
      io:format("Elf ~p: I am gonna kill someone! :) There are ~p elves left.~n", [ElfId, TotalElves]),
      OppositeElfId = TotalElves div 2,
      LeftElf ! {get_gifts, self(), OppositeElfId},
      killing(ElfId, LeftElf, RightElf, Gifts, MaxGifts, TotalElves);
    {get_gifts, From, 1} ->
%%      io:format("Elf ~p: I am dead :(~n", [ElfId]),
%%      io:format("Elf ~p: Telling ~p that ~p is now his neighbour to the right~n", [ElfId, LeftElf, RightElf]),
%%      io:format("Elf ~p: Telling ~p that ~p is now his neighbour to the left~n", [ElfId, RightElf, LeftElf]),
%%      io:format("Elf ~p: Giving my gifts to ~p~n", [ElfId, From]),
      LeftElf ! {right, RightElf},
      RightElf ! {left, LeftElf},
      From ! {gifts, Gifts};
    {get_gifts, From, N} ->
      LeftElf ! {get_gifts, From, N - 1},
      elf_proc2(ElfId, LeftElf, RightElf, Gifts, MaxGifts)
  end.

killing(ElfId, LeftElf, RightElf, Gifts, MaxGifts, TotalElves) ->
  receive
    {right, NewRightElf} ->
      killing(ElfId, LeftElf, NewRightElf, Gifts, MaxGifts, TotalElves);
    {left, NewLeftElf} ->
      killing(ElfId, NewLeftElf, RightElf, Gifts, MaxGifts, TotalElves);
    {gifts, MoreGifts} ->
      LeftElf ! {your_turn, TotalElves - 1},
      elf_proc2(ElfId, LeftElf, RightElf, Gifts + MoreGifts, MaxGifts)
  end.







