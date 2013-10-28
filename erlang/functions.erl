-module(functions).
-compile(export_all). %% replace with -export() later, for God's sake!

%% return the head of a list
%% functions:head([a,b,c,d]). %% a
head([H|_]) ->
    H.

%% return the second element of a list
%% functions:head([a,b,c,d]). %% b
second([_,X|_]) ->
    X.

%% return true if two elements are the same.
%% else false.
%% functions:same([a,b,c,d],[a,b,c,d]). %% true
%% functions:same([a,b,c,d],[a,b,c,e]). %% false
same(X,X) ->
    true;
same(_,_) ->
    false.

%% functions:valid_time({{2011,09,06},{09,04,43}}). 
%% The Date tuple ({2011,9,6}) says today is: 2011/9/6, The time tuple ({9,4,43}) indicates: 9:4:43.
%% functions:valid_time(a). %% Stop feeding me wrong data!
valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").

