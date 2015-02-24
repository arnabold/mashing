-module(tut).
-export([double/1,test/0]).

%% c(tut). to compile in erlang shell

double(X) ->
    2 * X.

test() ->
    20 = double(10).

