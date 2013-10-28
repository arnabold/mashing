-module(maps_filters_folds).
-compile(export_all).

%% map

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

%% only keep even numbers
%% maps_filters_folds:even([1,2,3,4,5,6]).%% [2,4,6]

even(L) -> lists:reverse(even(L,[])).
 
even([], Acc) -> 
    Acc;
%% when head is even
even([H|T], Acc) when H rem 2 == 0 ->
    even(T, [H|Acc]);
%% when head is not even (odd)
even([_|T], Acc) ->
    even(T, Acc).

%% only keep men older than 60
%% maps_filters_folds:old_men([{male,61},{xxx,62},{male,60},{male,100}]). %% [{male,61},{male,100}]
old_men(L) -> lists:reverse(old_men(L,[])).
 
old_men([], Acc) -> 
    Acc;
%% when man age is > 60
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
    old_men(People, [Person|Acc]);
%% when man age is not > 60
old_men([_|People], Acc) ->
    old_men(People, Acc).

%% filter

filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
 
filter(_, [], Acc) -> 
    Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true  -> filter(Pred, T, [H|Acc]);
        false -> filter(Pred, T, Acc)
    end.
 
test_filter() ->
    Numbers = lists:seq(1,10), %% [1,2,3,4,5,6,7,8,9,10]
    filter(fun(X) -> X rem 2 == 0 end, Numbers), %% [2,4,6,8,10]
    People = [{male,45},{female,67},{male,66},{female,12},{unkown,174},{male,74}], %% [{male,45},{female,67},{male,66},{female,12},{unkown,174},{male,74}]
    filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People), %% [{male,66},{male,74}]
    0.

%% fold

%% find the maximum of a list
max([H|T]) -> 
    max2(T, H).

max2([], Max) -> 
    Max;
max2([H|T], Max) when H > Max -> 
    max2(T, H);
max2([_|T], Max) -> 
    max2(T, Max).

%% find the minimum of a list
min([H|T]) -> 
    min2(T,H).

min2([], Min) -> 
    Min;
min2([H|T], Min) when H < Min -> 
    min2(T,H);
min2([_|T], Min) -> 
    min2(T, Min).

%% sum of all the elements of a list
sum(L) -> 
    sum(L,0).

sum([], Sum) -> 
    Sum;
sum([H|T], Sum) -> 
    sum(T, H+Sum).

fold(_, Start, []) -> 
    Start;
fold(F, Start, [H|T]) -> 
    fold(F, F(H,Start), T).

test_fold() ->
    [H|T] = [1,7,3,5,9,0,2,3],
    fold(fun(A,B) when A > B -> A; 
            (_,B) -> B end, 
         H, 
         T), %% 9
    fold(fun(A,B) when A < B -> A; 
            (_,B) -> B end, 
         H, 
         T), %% 0
    fold(fun(A,B) -> A + B end, 
         0, 
         lists:seq(1,6)), %% 21
    0.

reverse2(L) ->
    fold(fun(X,Acc) -> [X|Acc] end, [], L).

map2(F,L) ->
    reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

filter2(Pred, L) ->
    F = fun(X,Acc) ->
            case Pred(X) of
                true  -> [X|Acc];
                false -> Acc
            end
        end,
    reverse(fold(F, [], L)).
