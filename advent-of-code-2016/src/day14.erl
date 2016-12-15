-module(day14).

-compile(export_all).

-record(key, {key,
              character,
              ttl = 1000,
              index,
              confirmation_index}).

input() -> "abc".

solve() ->
  solve(0, input(), [], [], fun erlang:md5/1).

solve2() ->
  solve(0, input(), [], [], fun my_md5/1).

solve(N, Salt, PossibleKeys, CorrectKeys, HashGen) when length(CorrectKeys) < 64 ->
  Key = HashGen(Salt ++ integer_to_list(N)),
  {NewCorrectKeys, NewPossibleKeys} = check_possible_keys(Key, PossibleKeys, [], CorrectKeys),
  case is_possible_key(erlang:md5(Salt ++ integer_to_list(N))) of
    false -> solve(N + 1, Salt, NewPossibleKeys, NewCorrectKeys, HashGen);
    {true, C} -> solve(N + 1, Salt, [#key{key = Key, character = C, index = N} | NewPossibleKeys], NewCorrectKeys, HashGen)
  end;
solve(_N, _Salt, _PossibleKeys, CorrectKeys, _HashGen) ->
  lists:nth(64, lists:keysort(#key.index, CorrectKeys)).

is_possible_key(<<>>) ->
  false;
is_possible_key(<<A:4, A:4, A:4, _Rest/bits>>) ->
  {true, A};
is_possible_key(<<_:4, Rest/bits>>) ->
  is_possible_key(Rest).

check_possible_keys(Key, [#key{ttl = TTL, character = Char, key = PossKey} = KR | Rest], Acc, ConfirmedAcc) ->
  case is_confirmed_key(Char, Key) of
    true ->
      io:format("~p: Confirmed ~p  index ~p with ~p~n", [length(ConfirmedAcc) + 1,print_hash(PossKey), KR#key.index, print_hash(Key)]),
      check_possible_keys(Key, Rest, Acc, [KR | ConfirmedAcc]);
    false when TTL > 0 ->
      check_possible_keys(Key, Rest, Acc ++ [KR#key{ttl = TTL - 1}], ConfirmedAcc);
    false ->
      check_possible_keys(Key, Rest, Acc, ConfirmedAcc)
  end;
check_possible_keys(_, [], Acc, ConfirmedAcc) ->
  {ConfirmedAcc, Acc}.

is_confirmed_key(_Char, <<>>) ->
  false;
is_confirmed_key(Char, <<Char:4, Char:4, Char:4, Char:4, Char:4, _Rest/bits>>) ->
  true;
is_confirmed_key(Char, <<_:4, Rest/bits>>) ->
  is_confirmed_key(Char, Rest).

my_md5(Hash) ->
  Bin = erlang:md5(Hash),
  my_md5(2016, Bin).

my_md5(N, Hash) when N > 0 ->
  String = print_hash(Hash),
  my_md5(N - 1, erlang:md5(String));
my_md5(0, Hash) ->
  Hash.


print_hash(Bin) ->
  string:to_lower(lists:flatten([io_lib:format("~1.16.0B", [Char]) || <<Char:4>> <= Bin])).

