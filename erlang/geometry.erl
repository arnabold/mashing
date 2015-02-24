-module(geometry).
-export([area/1,test/0]).

%% the function area consist of two clauses separated by a ;
%% the function head consists of a pattern matched against the calling arguments
area({rectangle,Width,Ht}) ->
    Width * Ht;
area({circle,R}) ->
    3.14159*R*R.

%% tests
test() ->
    50 = area({rectangle,10,5}),
    6.157516399999999 = area({circle,1.4}),
    ok.
