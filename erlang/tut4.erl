-module(tut4).

-export([list_length/1,test/0]).

%% Lists

%% This is not tail recursive, there is a better way to write this function
list_length([]) ->
    0;    
list_length([_ | Rest]) ->
    1 + list_length(Rest).

%% In general we can say we use tuples where we would use "records" or "structs" in other 
%% languages and we use lists when we want to represent things which have varying sizes, 
%% (i.e. where we would use linked lists in other languages).

test() ->
    _Temperatures =  [{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, 
                     {paris, {f, 28}}, {london, {f, 36}}],
    [First|TheRest] = [1,2,3,4,5],
    First = 1,
    TheRest = [2,3,4,5],
    [E1, E2 | R] = [1,2,3,4,5,6,7],
    E1 = 1,
    E2 = 2,
    R = [3,4,5,6,7],
    [A, B | C] = [1, 2],
    A = 1,
    B = 2,
    C = [],
    7 = list_length([1,2,3,4,5,6,7]),
    %% Strings are character list
    "abc" = [97,98,99].
