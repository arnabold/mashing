-module(tut6).
-export([list_max/1,list_min/1,test/0]).

%% Matching, Guards and Scope of Variables

%% list_max/1
list_max([Head|Rest]) ->
    %% assumes that the max value of the list is Head
    list_max(Rest, Head).

%% list_max/2
list_max([], Res) ->
    Res;
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    list_max(Rest, Head);
list_max([_|Rest], Result_so_far)  ->
    list_max(Rest, Result_so_far).

%% list_min/1
list_min([Head|Rest]) ->
    %% assumes that the min value of the list is Head
    list_min(Rest, Head).

%% list_min/2
list_min([], Res) ->
    Res;
list_min([Head|Rest], Result_so_far) when Head < Result_so_far ->
    list_min(Rest, Head);
list_min([_|Rest], Result_so_far)  ->
    list_min(Rest, Result_so_far).

test() ->
    7 = tut6:list_max([1,2,3,4,5,7,4,3,2,1]),
    1 = tut6:list_min([7,2,3,4,5,1,4,3,2,7]),
    M = 5,
    %% M = 6, %% ** exception error: no match of right hand side value 6
    %% M = M + 1, %% ** exception error: no match of right hand side value 6
    N = M + 1,
    6 = N,
    {X, Y} = {paris, {f, 28}},
    X = paris,
    Y = {f,28},
    %% {X, Y} = {london, {f, 36}} %% ** exception error: no match of right hand side value {london,{f,36}
    ok.
    

