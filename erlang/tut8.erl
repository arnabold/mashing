-module(tut8).

-export([reverse/1,test/0]).

reverse(List) ->
    reverse(List, []).

reverse([Head | Rest], Reversed_List) ->
    %% take off the heads of the list to be reversed and 
    %% add them to the the Reversed_Lis
    reverse(Rest, [Head | Reversed_List]); 
reverse([], Reversed_List) ->
    Reversed_List.

test() ->
    [M1|T1] = [paris, london, rome],
    M1 = paris,
    T1 = [london,rome],
    L1 = [madrid|T1],
    L1 = [madrid,london,rome],
    [3,2,1] = reverse([1,2,3]),
    [3,2,1] = lists:reverse([1,2,3]),
    ok.
