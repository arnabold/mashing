-module(startingout).

-export([numbers/0, variables/0, theequaloperator/0, atoms/0, comparisions/0, tuples/0, lists/0, 
         listcomprehension/0, bitsyntax/0, binarycomprehension/0]).

%% How to compile the file:
%% in repl: c("startingout.erl").
%% in shell: erlc startingout.erl

%% How to call functions:
%% startingout:theequaloperator().
%% startingout:variables().
%% startingout:numbers().
%% ...

%% To clear all the variable bindings in repl: f().

%% Numbers
numbers() ->
    %% _ is the always unbound variable
    _ = 2 + 15,
    _ = 49 * 100,
    _ = 1892 -1472,
    _ = 5 / 2, %% 2.5
    _ = 5 div 2, %% 2
    _ = 5 rem 2, %% 1
    _ = (50 * 100) - 4999, %% 1
    _ = -(50 * 100 - 4999), %% -1
    _ = -50 * (100 - 4999), %% 244950
    _ = 2#101010, %% 42
    _ = 8#0677, %% 447
    _ = 16#AE, %% 174
    0. 

%% Invariable Variables
%% Variables name must begin with a capital letter
variables() ->
    %% One, %% error: variable One is unbound
    One = 1,
    Un = Uno = One = 1, %% 1
    Un = Un, %% just to suppress the warning Un is unused
    Uno = Uno, %% just to suppress the warning Uno is unused
    Two = One + One, %% 2
    Two = 2, %% 2
    %% Two = Two + 1, %% error: no match of right hand side value 3
    %% two = 2, %% error: no match of right end side value 2. two is an atom.
    two = two, %% two
    0.

theequaloperator() ->
    47 = 45 + 2,
    %% 47 = 45 + 3, %% error: no match of right hand side value 48
    0.

%% Atoms
%% everything in single quotes or beginning with a lowercase letter
atoms() ->
    atom,
    atoms_rule,
    atoms_rule@erlang,
    'Atoms can be cheated!',
    atoms = 'atoms',
    0.

%% Boolean Algebra & Comparision operators
comparisions() ->
    %% and and or are not short circuits
    _ = true and false, %% false
    _ = false or true, %% true
    _ = true xor false, %% true
    _ = not false, %% true
    _ = not (true and true), %% false
    %% andalso and orelse are short circuits
    _ = false andalso true, %% false
    _ = true orelse false, %% true
    %% testing for equality or inequality
    _ = 5 =:= 5, %% true
    _ = 1 =:= 0, %% false
    _ = 1 =/= 0, %% true
    _ = 5 =:= 5.0, %% false
    _ = 5 == 5.0, %% true (for == doesn't matter if arguments are different types)
    _ = 5 /= 5.0, %% false
    _ = 1 < 2, %% true
    _ = 1 < 1, %% false
    _ = 1 >= 1, %% true
    _ = 1 =< 1, %% true (attention! is not <=)
    %% _ = 5 + llama, %% error: bad argument in arithmetic expression
    _ = 5 =:= true, %% false (attention! different types here)
    _ = 0 == false, %% false (0 is not false)
    _ = 0 == true, %% false (and 0 is not true)
    _ = 1 < false, %% true. !!! The correct ordering of each element in a comparison is the following: 
    %% number < atom < reference < fun < port < pid < tuple < list < bit string
    %% false and true are atoms.
    0.

%% Tuples
tuples() ->
    A = 10,  B = 4,
    Punto = {A,B}, %% {10,4} a tuple
    _ = Punto,
    Point = {4,5},
    {X,Y} = Point, %% {4,5}
    _ = X, %% 4
    _ = Y, %% 5
    {X,_} = Point, %% {4,5} pattern matching but we do not need to use the second element
    {_,_} = {4,5}, %% {4,5}
    %% {_,_} = {4,5,6}, %% error: no match of right hand side value {4,5,6}
    Temperature = 23.213, %% 23.213
    _ = Temperature,
    PreciseTemperature = {celsius, 23.213}, %% {celsius,23.213}
    _ = PreciseTemperature,
    %% {kelvin, T} = PreciseTemperature, %% error: no match of right hand side value {celsius,23.213}
    %% A tuple which contains an atom with one element following it is called a 'tagged tuple'.
    {point, {4,5}} = {point, {X,Y}}, %% {point,{4,5}}
    0.

%% Lists
lists() ->
    _ = [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom], %% [1,2,3,{numbers,[4,5,6]},5.34,atom]
    _ = [97, 98, 99], %% "abc"
    _ = [97,98,99,4,5,6], %% [97,98,99,4,5,6]
    _ = [233], %% "é"
    %% Erlang will print lists of numbers as numbers only when at least one of them could not also represent a 
    %% letter!
    %% To glue lists together, we use the ++ operator
    _ = [1,2,3] ++ [4,5], %% [1,2,3,4,5]
    _ = [1,2,3,4,5] -- [1,2,3], %% [4,5]
    _ = [2,4,2] -- [2,4], %% [2]
    _ = [2,4,2] -- [2,4,2], %% []
    %% Both ++ and -- are right-associative.
    _ = [1,2,3] -- [1,2] -- [3], %% [1,2,3] -- [1,2] -> [3]
    _ = [1,2,3] -- [1,2] -- [2], %% [1,2,3] -- [1] -> [2,3]
    %% Head
    _ = hd([1,2,3,4]), %% 1
    %% Tail
    _ = tl([1,2,3,4]), %% [2,3,4]
    %% Length
    _ = length([1,2,3,4]), %% 4
    List = [2,3,4],
    NewList = [1|List], %% [1,2,3,4]
    [Head|Tail] = NewList,
    _ = Head, %% 1
    _ = Tail, %% [2,3,4]
    [NewHead|NewTail] = Tail,
    _ = NewHead, %% 2
    _ = NewTail, %% [3,4]
    %% The | we used is named the cons operator (constructor).
    _ = [1 | [] ], %% [1]
    _ = [2 | [1 | [] ] ], %% [2,1]
    _ = [3 | [2 | [1 | [] ] ] ], %% [3,2,1]
    [a, b, c, d] = 
        [a, b, c, d | []] = 
        [a, b | [c, d]] = 
        [a, b | [c | [d]]] = 
        [a | [b | [c | [d]]]] = 
        [a | [b | [c | [d | [] ]]]], %% [a,b,c,d]
    %% improper lists
    _ = [1|2], %% [1|2] and not [1,2]!!!
    %% _ = length([1|2]), %% error: bad argument
    _ = length ([1|[2]]), %% 2
    %% Proper lists end with an empty list as their last cell.
    0.

%% List Comprehension
listcomprehension() ->
    %% the idea of the set notation. i.e.: all positive numbers {x : x > 0}
    %% {2n : n in L} and L is 1,2,3,4
    _ = [2*N || N <- [1,2,3,4]], %% [2,4,6,8]
    _ = [X || X <- [1,2,3,4,5,6,7,8,9,10], 
              X rem 2 =:= 0], %% [2,4,6,8,10]
    RestaurantMenu = [{steak, 5.99}, 
                      {beer, 3.99}, 
                      {poutine, 3.50}, 
                      {kitten, 20.99}, 
                      {water, 0.00}], %% [{steak,5.99}, {beer,3.99}, {poutine,3.5}, {kitten,20.99}, {water,0.0}]
    _ = [{Item, Price * 1.07} || {Item,Price} <- RestaurantMenu,
                             Price >= 3,
                             Price =< 10], %% [{steak,6.409300000000001}, {beer,4.2693}, {poutine,3.745}]
    %% list comprehensions:
    %% NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN]. 
    %% The part Pattern <- List is named a Generator expression. You can have more than one!
    [X+Y || X <- [1,2], Y <- [3,5]], %% [4,6,5,7]
    %% the generator expressions coupled with pattern matching also act as a filter
    Weather = [{toronto, rain}, {montreal, storms}, {london, fog},   
               {paris, sun}, {boston, fog}, {vancouver, snow}], %% [{toronto,rain}, {montreal,storms}, {london,fog},
    %% {paris,sun}, {boston,fog}, {vancouver,snow}]
    FoggyPlaces = [X || {X, fog} <- Weather], %% [{london,fog},{boston,fog}]
    _ = FoggyPlaces,
    0.

%% Bit Syntax
bitsyntax() ->
    Color = 16#F09A29, %% 15768105. color in hex notation 16#RRGGBB. Here a tint of orange
    Pixel = <<Color:24>>, %% <<240,154,41>>. 24 bit representation: each segment is 8 bits
    _ = Pixel,
    Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>, %% 12x8 bit string
    <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels, %% <<213,45,132,64,76,32,76,0,0,234,32,15>>
    _ = Pix1, %% 13970820. A 24 bit string
    _ = Pix2, %% 4213792
    _ = Pix3, %% 4980736
    _ = Pix4, %% 15343631
    <<R:8, G:8, B:8>> = <<Pix1:24>>, %% <<213,45,132>>. A 3*8 bit string
    _ = R, %% 213
    _ = G, %% 45
    _ = B, %% 132
    <<R:8, Rest/binary>> = Pixels,
    _ = R, %% 213
    _ = Rest, %% <<45,132,64,76,32,76,0,0,234,32,15>>
    <<X1/unsigned>> = <<-42>>, %% <<"Ö">> (214)
    _ = X1,
    <<X2/signed>> = <<-42>>, %% <<"Ö">> (-42)
    <<X2/integer-signed-little>> =  <<-42>>, %% <<"Ö">> (-42)
    <<N:8/unit:1>> = <<72>>, %% <<"H">> (72)
    <<N/integer>> = <<72>>, %% <<"H">> (72)
    <<Y:4/little-unit:8>> = <<72,0,0,0>>, %% <<72,0,0,0>> (72)
    _ = Y,
    %% binary operations
    2#00100 = 2#00010 bsl 1, %% 4 
    2#00001 = 2#00010 bsr 1, %% 1
    2#10011 = 2#10001 bor 2#0010, %% 19
    %% parsing TCP segments 
    <<SourcePort:16, DestinationPort:16,AckNumber:32,DataOffset:4, _Reserved:4, Flags:8, WindowSize:16,
      CheckSum: 16, UrgentPointer:16,Payload/binary>> = <<"657483265784326758436278564783256849">>,
    _ = SourcePort,
    _ = DestinationPort,
    _ = AckNumber,
    _ = DataOffset,
    _ = _Reserved,
    _ = Flags,
    _ = WindowSize,
    _ = CheckSum,
    _ = UrgentPointer,
    _ = <<Payload/binary>>,
    0.

%% Binary Comprehension
binarycomprehension() ->
    [ X || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0], %% [2,4]
    Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>,
    RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ], %% [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]
    << <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>, %% <<213,45,132,64,76,32,76,0,0,234,32,15>>
    << <<Bin/binary>> || Bin <- [<<3,7,5,4,7>>] >>, %% <<3,7,5,4,7>>
    << <<(X+1)/integer>> || <<X>> <= <<3,7,5,4,7>> >>, %% <<4,8,6,5,8>>
    0.
