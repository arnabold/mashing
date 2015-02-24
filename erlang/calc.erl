-module(calc).

-export([rpn/1, spikes/0, rpn_test/0]).

spikes() ->
    L = string:tokens("10 4 3 + 2 * -", " "), %% ["10","4","3","+","2","*","-"]
    _ = lists:foldl(fun(S1,S2) -> [S1|S2] end , [], string:tokens(L, " ")), %% [["10","4","3","+","2","*","-"]]
    _ = string:to_float("10"), %% {error,no_float}
    _ = list_to_integer("10"), %% 10
    _ = string:to_float("+"), %% {error,no_float}
    _ = string:to_float("1.1"), %% {1.1,[]}
    _ = rpn("10"), %% 10
    _ = rpn("1 1 +"), %% 2
    _ = calc:rpn("3 5 +"), %% 8
    _= calc:rpn("7 3 + 5 +"), %% 15
    ok.

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.
    
%% if "+" in the stack, take the following two elements, sum them and put the result in the stack
rpn("+", [N1,N2|S]) ->
    [N2+N1|S];
%% other operations
rpn("-", [N1,N2|S]) -> [N2-N1|S];
rpn("*", [N1,N2|S]) -> [N2*N1|S];
rpn("/", [N1,N2|S]) -> [N2/N1|S];
rpn("^", [N1,N2|S]) -> [math:pow(N2,N1)|S];
rpn("ln", [N|S])    -> [math:log(N)|S];
rpn("log10", [N|S]) -> [math:log10(N)|S];
rpn("sum", Stack)   -> [lists:sum(Stack)];
rpn("prod", Stack)  -> [lists:foldl(fun erlang:'*'/2, 1, Stack)];
%% parse the element (should be a number) and put in the stack
rpn(X,Stack) ->
    [read(X)|Stack].

read(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_} -> F
    end.

rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    %% Here the result is not a single element
    ok = try
        rpn("90 34 12 33 55 66 + * - +") %% error: no match of right hand side value [-3947,90]
    catch
        error:{badmatch,[_|_]} -> ok
    end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 =  rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    50 = rpn("10 10 10 20 sum"),
    10.0 = rpn("10 10 10 20 sum 5 /"),
    1000.0 = rpn("10 10 20 0.5 prod"),
    ok.
