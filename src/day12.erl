-module(day12).

-compile(export_all).

solve(InputFile) ->
  {ok, Fd} = file:open(InputFile, [read, raw, binary]),
  {ok, Line} = file:read_line(Fd),
  DecodedJson = jsx:decode(Line),
  process_day12(DecodedJson, 0).

process_day12([Int | T], Sum) when is_integer(Int) ->
  process_day12(T, Sum + Int);
process_day12([List | T], Sum) when is_list(List) ->
  process_day12(T, Sum + process_day12(List, 0));
process_day12([{_Key, Val} | T], Sum) ->
  process_day12([Val | T], Sum);
process_day12([_ | T], Sum) ->
  process_day12(T, Sum);
process_day12([], Sum) ->
  Sum.

solve2(InputFile) ->
  {ok, Fd} = file:open(InputFile, [read, raw, binary]),
  {ok, Line} = file:read_line(Fd),
  DecodedJson = jsx:decode(Line),
  process_day12_2(DecodedJson, 0).

process_day12_2([Int | T], Sum) when is_integer(Int) ->
  process_day12_2(T, Sum + Int);
process_day12_2([List | T], Sum) when is_list(List) ->
  case lists:keymember(<<"red">>, 2, List) of
    false ->
      process_day12_2(T, Sum + process_day12_2(List, 0));
    true ->
      process_day12_2(T, Sum)
  end;
process_day12_2([{_Key, Val} | T], Sum) ->
  process_day12_2([Val | T], Sum);
process_day12_2([_ | T], Sum) ->
  process_day12_2(T, Sum);
process_day12_2([], Sum) ->
  Sum.