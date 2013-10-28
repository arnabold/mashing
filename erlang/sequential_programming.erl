-module(sequential_programming).
-compile(export_all).

integers() ->
    _ = 10,
    _ = -234,
    _ = 16#AB10F, %% an integer in base 16
    _ = 2#110111010, %% an integer in base 2
    $A. %% an integer expressed as an ascii value

floats() ->
    _ = 17.368,
    _ = -56.654,
    12.34E-10.

atoms() ->
    abcef,
    start_with_a_lower_case_letter, %% atoms start with a lowercase letter
    'Blanks can be quoted', %% characters between quotes are atoms
    'Anything inside quotes \n\012'.

tuples() ->
    _ = {123, bcd},
    _ = {123, def, abc},
    _ = {person, 'Joe', 'Armstrong'},
    _ = {abc, {def, 123}, jkl},
    _ = {}.

lists() ->
    _ = [123, xyz],
    _ = [123, def, abc],
    _ = [{person, 'Joe', 'Armstrong'},
     {person, 'Robert', 'Virding'},
     {person, 'Mike', 'Williams'}],
    %% strings are list of characters
    [97,98,99,100,101,102,103,104,105] = "abcdefghi",
    [] = "".

variables() ->
    Abc = 0, %% variables start with uppercase letter
    A_long_variable_name = 0,
    AnObjectOrientedVariableName = 0,
    _ = Abc,
    _ = A_long_variable_name,
    _ = AnObjectOrientedVariableName.

complex_data_structures() ->
    _ = [{{person,'Joe', 'Armstrong'},
          {telephoneNumber, [3,5,9,7]},
          {shoeSize, 42},
          {pets, [{cat, tubby},{cat, tiger}]},
          {children,[{thomas, 5},{claire,1}]}},
         {{person,'Mike','Williams'},
          {shoeSize,41},
          {likes,[boats, beer]}}].

pattern_matching() ->
    A = 10,
    {B, C, D} = {10, foo, bar},
    {A2, A2, B2} = {abc, abc, foo},
    %% {A3, A3, B3} = {abc, def, 123}, %% ** exception error: no match of right hand side value {abc,def,123}
    [A4,B4,C4] = [1,2,3],
    %% [A5,B5,C5,D5] = [1,2,3], %% ** exception error: no match of right hand side value [1,2,3]
    10 = A, 10 = B, foo = C, bar = D, abc = A2, 
    foo = B2, 1 = A4, 2 = B4, 3 = C4,
    [A6,B6|C6] = [1,2,3,4,5,6,7],
    [H|T] = [1,2,3,4],
    [H2|T2] = [abc],
    %% [H3|T3] = [], %% ** exception error: no match of right hand side value []
    {A7,_,[B7|_],{B7}} = {abc,23,[22,x],{22}},
    1 = A6, 2 = B6, [3,4,5,6,7] = C6, 1 = H, 
    [2,3,4] = T, abc = H2, [] = T2, 
    abc = A7,
    [H4|T4] = [1,2,3,4],
    [H5|T5] = [abc], 
    %% [H6|T6] = [], %% ** exception error: no match of right hand side value []
    {A8,_, [B8|_],{B8}} = {abc,23,[22,x],{22}},
    1 = H4, [2,3,4] = T4, abc = H5, [] = T5, abc = A8.

function_calls(Arg1, Arg2, Arg3) ->
    _ = Arg1,
    _ = Arg2,
    _ = Arg3,
    ok.

double(X) ->
    times(X, 2).

times(X, N) ->
    X * N.

module_system() ->
    double(1).

starting_the_systems() ->
    %% $ erl
    %% 1> c(module_name).
    %% 2> module_name:function_name().
    ok.

built_in_functions_bifs() ->
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    5 = length([1,2,3,4,5]),
    3 = size({a,b,c}),
    "an_atom" = atom_to_list(an_atom),
    {1, 2, 3, 4} = list_to_tuple([1,2,3,4]),
    "2234" = integer_to_list(2234),
    [] = tuple_to_list({}), 
    _ = Year, _ = Month, _ = Day, _ = Hour, 
    _ = Minute, _ = Second,
    ok.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

area({square,Side}) ->
    Side * Side;
area({circle,Radius}) ->
    3.14 * Radius * Radius; %% inexact
area({triangle, A, B, C}) ->
    S = (A + B + C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C));
area(Other) ->
    {invalis_object, Other}.

function_syntax() ->
    120 = factorial(5),
    4 = area({square,2}),
    12.56 = area({circle,2}),
    2.9047375096555625 = area({triangle,2,3,4}),
    {invalis_object,{rectangle,2,3}} = area({rectangle,2,3}).

test() ->
    integers(),
    floats(),
    atoms(),
    tuples(),
    lists(),
    variables(),
    complex_data_structures(),
    pattern_matching(),
    sequential_programming:function_calls(1, "hello", 
                                          {world}),
    module_system(),
    built_in_functions_bifs(),
    function_syntax().
