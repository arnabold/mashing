-module(tut_wot).
-export([test/0]).

test() ->
    ok = io:format("hello world~n",[]),
    ok = io:format("this outputs one Erlang term: ~w~n",[hello]),
    ok = io:format("this outputs two Erlang terms: ~w~w~n",[hello,world]),
    ok = io:format("this outputs two Erlang terms: ~w ~w~n",[hello,world]).

