-module(tut3).
-export([convert_length/1,test/0]).

%% Tuples

convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.

test() ->
    {centimeter,12.7} = convert_length({inch,5}),
    {inch,5.0} = convert_length(convert_length({inch,5})).
