-module(tut13).

-export([convert_list_to_c/1,test/0]).

%% High Order Functions (HOFs)

%% These two functions are provided in the standard module lists. 
%% foreach takes a list and applies a fun to every element in the list, 
%% map creates a new list by applying a fun to every element in a list.

%% foreach(Fun, [First|Rest]) ->
%%     Fun(First),
%%     foreach(Fun, Rest);
%% foreach(Fun, []) ->
%%     ok.

%% map(Fun, [First|Rest]) -> 
%%     [Fun(First)|map(Fun,Rest)];
%% map(Fun, []) -> 
%%     [].

convert_to_c({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_c(List) ->
    %% The standard module lists also contains a function sort(Fun, List) where Fun is a fun 
    %% with two arguments. This fun should return true if the the first argument is less than 
    %% the second argument, or else false.
    New_list = lists:map(fun convert_to_c/1, List),
    lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) ->
                       Temp1 < Temp2 end, New_list).


test() ->
    Xf = fun(X) -> X * 2 end,
    10 = Xf(5),
    Add_3 = fun(X) -> X + 3 end,
    [4,5,6] = lists:map(Add_3,[1,2,3]),
    Print_City = fun({City,{X,Temp}}) -> io:format("~-15w ~w ~w~n",[City,X,Temp]) end,
    ok = lists:foreach(Print_City,[{moscow, {c, -10}}, {cape_town, {f, 70}},{stockholm, {c, -4}}, 
                                   {paris, {f, 28}}, {london, {f, 36}}]),
    [{moscow,{c,-10}},{stockholm,{c,-4}},{paris,{c,-2}},{london,{c,2}},{cape_town,{c,21}}] =
        convert_list_to_c([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, 
                           {paris, {f, 28}}, {london, {f, 36}}]),
    ok.

