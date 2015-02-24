-module(tut9).
-export([test_if/2,test/0]).

%% If

test_if(A, B) ->
    if 
        A == 5 ->
            io:format("A == 5~n", []),
            a_equals_5;
        B == 6 ->
            io:format("B == 6~n", []),
            b_equals_6;
        A == 2, B == 3 ->                      %i.e. A equals 2 and B equals 3
            io:format("A == 2, B == 3~n", []),
            a_equals_2_b_equals_3;
        A == 1 ; B == 7 ->                     %i.e. A equals 1 or B equals 7
            io:format("A == 1 ; B == 7~n", []),
            a_equals_1_or_b_equals_7
    end.

test() ->
    a_equals_5 = test_if(5,33),
    b_equals_6 = test_if(33,6),
    a_equals_2_b_equals_3 = test_if(2, 3),
    a_equals_1_or_b_equals_7 = test_if(1, 33),
    a_equals_1_or_b_equals_7 = test_if(33, 7),
    %% test_if(33, 33), %% ** exception error: no true branch found when evaluating an if expression
    ok.
