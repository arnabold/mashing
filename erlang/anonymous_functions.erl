-module(anonymous_functions).
-compile(export_all).

anonymous_functions() ->
    Fn = fun() -> a end,
    Fn(), %% a
    L = [1,2,3,4,5],
    lists:map(fun(X) -> X + 1 end, L), % [2,3,4,5,6]
    lists:map(fun(X) -> X - 1 end, L), % [0,1,2,3,4]
    PrepareAlarm =
        fun(Room) ->
                io:format("Alarm set in ~s.~n", [Room]),
                fun() ->
                        io:format("Alarm tripped in ~s! Call Batman!~n", [Room]) end
                    end, %% #Fun<erl_eval.6.82930912>
    AlarmReady = PrepareAlarm("bathroom"), %% #Fun<erl_eval.20.82930912>
    AlarmReady(), %% ok
    0.

%% anonymous_functions:base(3) %% 12
base(A) ->
    B = A + 1,
    F = fun() -> A * B end,
    F().

%% anonymous_functions:base2(3) %% 12
%% base2(A) ->
%%     B = A + 1,
%%     F = fun() -> C = A * B end,
%%     F(),
%%     C. %% C is unbound

a() ->
    Secret = "pony",
    fun() -> Secret end.

b(F) ->
    "a/0's password is " ++ F().

%% anonymous_functions:b(anonymous_functions:a()). %% "a/0's password is pony"
            
%% closures

closures() ->
    Base = 2,
    PowerOfTwo = fun(X) -> math:pow(Base,X) end,
    lists:map(PowerOfTwo,[1,2,3,4]), %% [2.0,4.0,8.0,16.0]
    0.

foo() ->
    A = 1,
    (fun() -> A = 2 end) (). %% error: no match of right hand side value 2

bar() ->
    A = 1,
    (fun(A) -> A = 2 end)(2). %% Warning: variable 'A' shadowed in 'fun'

              
                         
