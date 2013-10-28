-module(what_the_if).
-export([heh_fine/0, oh_god/1, help_me/1]).

%% error: no true branch found when evaluating an if expression
heh_fine() ->
    if 1 =:= 1 ->
        works
    end,
    if 1 =:= 2; 1 =:= 1 ->
        works
    end,
    if 1 =:= 2, 1 =:= 1 -> %% always false
        fails
    end.

%% what_the_if:oh_god(2). %% might_succeed
%% what_the_if:oh_god(1). %% always_does
oh_god(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does  %% this is Erlang's if's 'else!'
    end.

%% note, this one would be better as a pattern match in function heads!
%% I'm doing it this way for the sake of the example.
%% what_the_if:help_me(cat). %% {cat,"says meow!"}
%% what_the_if:help_me(a). %% {a,"says fgdadfgna!"}
help_me(Animal) ->
    Talk = if Animal == cat  -> "meow";
              Animal == beef -> "mooo";
              Animal == dog  -> "bark";
              Animal == tree -> "bark";
              true -> "fgdadfgna"
           end,
    {Animal, "says " ++ Talk ++ "!"}.

