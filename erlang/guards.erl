-module(guards).
-export([old_enough/1, right_age/1, wrong_age/1]).

%% guards:old_enough(16). %% true
%% guards:old_enough(15). %% false
old_enough(X) when X >= 16 -> 
    true;
old_enough(_) -> 
    false.

%% guards:right_age(104). %% true
%% guards:right_age(105). %% false
right_age(X) when X >= 16, X =< 104 -> %% The comma (,) acts in a similar manner to the operator andalso 
    true;
right_age(_) ->
    false.

%% guards:wrong_age(105). %% true
%% guards:wrong_age(104). %% false
wrong_age(X) when X < 16; X > 104 -> %% the semicolon (;) acts a bit like orelse
    true;
wrong_age(_) ->
    false.


