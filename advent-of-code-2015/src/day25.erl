-module(day25).

-compile(export_all).

-define(START, 20151125).

solve(Start, X, Y) ->
 find_code(1, 1, Start, X, Y).


find_code(TargetX, TargetY, Previous, TargetX, TargetY) ->
  Previous;
find_code(X, 1, Previous, TargetX, TargetY) ->
  find_code(1, X + 1, (Previous * 252533) rem 33554393, TargetX, TargetY);
find_code(X,Y, Previous, TargetX, TargetY) ->
  find_code(X + 1, Y - 1, (Previous * 252533) rem 33554393, TargetX, TargetY).

