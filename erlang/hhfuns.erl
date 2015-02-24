-module(hhfuns).
-compile(export_all).

one() ->
    1.
two() ->
    2.
%% hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0). %% 3
add(X,Y) ->
    X() + Y().

increment([]) ->
    [];
increment([Head|Tail]) ->
    [Head + 1|increment(Tail)].

decrement([]) ->
    [];
decrement([Head|Tail]) ->
    [Head - 1|decrement(Tail)].

map(_,[]) ->
    [];
map(Function,[Head|Tail]) ->
    [Function(Head)|map(Function,Tail)].

incr(X) ->
    X + 1.

decr(X) ->
    X - 1.

incr_list(List) ->
    map(fun incr/1, List).

decr_list(List) ->
    map(fun decr/1, List).

