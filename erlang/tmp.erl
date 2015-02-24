-module(tmp).
-compile(export_all).

loop(0) ->
    0;
loop(N) when N > 0 ->
    io:format("."),
    loop(N-1).


