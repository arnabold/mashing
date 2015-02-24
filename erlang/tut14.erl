-module(tut14).

-export([start/0, say_something/2,test/0]).

%% Processes

say_something(_, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p from ~p~n", [What,self()]),
    say_something(What, Times - 1).

start() ->
    _ = spawn(tut14, say_something, [hello, 3]), %% returns a pid
    _ = spawn(tut14, say_something, [goodbye, 3]). %% returns a pid

test() ->
    done = say_something(hello,3),
    _ = start(), %% a pid
    ok.

