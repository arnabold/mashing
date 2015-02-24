-module(errors).
-compile(export_all).

%% Runtime errors

function_clause() ->
    lists:sort([3,2,1]),
    lists:sort(fffffff), %% error: no function clause matching lists:sort(fffffff)
    0.

%% clause_case() ->
%%     case "Unexpected Value" of 
%%         expected_value -> ok;
%%         other_expected_value -> 'also ok'
%%     end, %% error: no case clause matching "Unexpected Value"
%%     0.

%% if_clause() ->
%%     if 2 > 4 -> ok;
%%        0 > 1 -> ok
%%     end,
%%     0. %% error: no true branch found when evaluating an if expression

%% badmatch() ->
%%     [X,Y] = {4,5}, %% error: no match of right hand side value {4,5}
%%     0.

%% badarg() ->
%%     erlang:binary_to_list("heh, already a list"), %% error: bad argument
%%     0.

undef() ->
    lists:random([1,2,3]), %% error: undefined function lists:random/1
    0.

%% badarith() ->
%%     5 + llama, %% error: bad argument in an arithmetic expression
%%     0.

add(X,Y) -> 
    X() + Y().
badfun() ->
    add(one,two), %%error: bad function one
    0.

badarity() ->
    F = fun(_) -> ok end,
    F(a,b), %% error: errors:'-badarity/0-fun-0-'/1 called with two arguments
    0.

%% Raising Exceptions

errors() ->
    %% erlang:error(badarith), %% error: bad argument in an arithmetic expression
    %% erlang:error(custom_error), %% error: custom_error
    0.

throws() ->
    %% throw(permission_denied), %% throw: permission_denied
    0.

%% Dealing with exceptions
