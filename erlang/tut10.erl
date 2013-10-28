-module(tut10).
-export([convert_length/1,test/0]).

%% Case

convert_length(Length) ->
    case Length of
        {centimeter, X} ->
            {inch, X / 2.54};
        {inch, Y} ->
            {centimeter, Y * 2.54}
    end.

test() ->
    {centimeter,15.24} = convert_length({inch, 6}),
    {inch,0.984251968503937} = convert_length({centimeter, 2.5}).
