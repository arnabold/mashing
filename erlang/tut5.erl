-module(tut5).
-export([format_temps/1,test/0]).

%% Only this function is exported
format_temps([])->                        % No output for an empty list
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest). %% loop (with recursion)

convert_to_celsius({Name, {c, Temp}}) ->  % No conversion needed
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->  % Do the conversion
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).

test() ->
    format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},{stockholm, {c, -4}}, 
                  {paris, {f, 28}}, {london, {f, 36}}]).
    
