-module(day19).

-compile(export_all).

input() -> 3012603.

test_input() -> 5.

solve() ->
  {ElfNo1, _AllElves} = spawn_elves(input(), [], input()),
  register(host, self()),
  ElfNo1 ! your_turn,
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
      LeftElf ! get_gifts,
      receive
        {gifts, MoreGifts, NewLeftElf} ->
          RightElf ! your_turn,
          elf_proc(ElfId, NewLeftElf, RightElf, Gifts + MoreGifts, MaxGifts)
      end;
    get_gifts ->
      LeftElf ! {right, RightElf},
      RightElf ! {gifts, Gifts, LeftElf}
  end.







