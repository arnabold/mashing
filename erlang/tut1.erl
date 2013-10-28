-module(tut1).
-export([fac/1,mult/2,test/0]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N-1).

mult(X,Y) ->
    X * Y.

test() ->
    24 = fac(4),
    12 = mult(3,4).
