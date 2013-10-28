-module(road).
-compile(export_all).

%% The road is laid in the pattern: A1, B1, X1, A2, B2, X2, ..., An, Bn, Xn, 
%% where X is one of the roads joining the A side to the B side of the map. 
%% We insert a 0 as the last X segment, because no matter what we do we're at our destination already. 
%% Data can probably be organized in tuples of 3 elements (triples) of the form {A,B,X}.

spikes() ->
%% find best case.
%% consider a single tuple A,B and choose the shortest
%% consider the following case:
%% A1 = 5, B1 = 1, X1 = 3, A2 = 10, B2 = 15, X2 = 0.
%% Choises to reach A1 point:
%% 1) A1 = 5
%% 2) B1 + X1 = 4 (shortest)
%% Choises to reach B1 point:
%% 1) B1 = 1 (shortest)
%% 2) A1 + X1 = 6
%% Choises to reach A2
%% 1) B1 + X1 + A2 = 14 (shortest)
%% 2) B1 + B2 + X2 = 16    
%% Choises to reach B2
%% 2) B1 + X1 + A2 + X2 = 14 (shortest)
%% 1) B1 + B2 = 16

%% file manipulation
    {ok, Binary} = file:read_file("road.txt"), %% {ok,<<"50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0\n">>}
    S = string:tokens(binary_to_list(Binary), "\r\n\t "), %% ["50","10","30","5","90","20","40","2","25","10","8","0"]
    R = [list_to_integer(X) || X <- S], % [50,10,30,5,90,20,40,2,25,10,8,0]
    R.

main(FileName) ->
    {ok,Bin} = file:read_file(FileName),
    Map = parse_map(Bin),
    io:format("~p~n", [optimal_path(Map)]),
    %% erlang:halt(0)
    ok.

parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
    Values = [list_to_integer(X) || X <- string:tokens(Str,"\r\n\t ")],
    group_vals(Values, []).

group_vals([], Acc) ->
    lists:reverse(Acc);
group_vals([A,B,X|Rest], Acc) ->
    group_vals(Rest, [{A,B,X} | Acc]).

%% accumulator: {{DistA,PathA}, {DistB,PathB}}
%% return {short_distance_to_A, short_distance_to_B}
shortest_step({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->
    OptA1 = {DistA + A, [{a,A}|PathA]}, %% option from An to An+1 using A
    OptA2 = {DistB + B + X, [{x,X}, {b,B}|PathB]}, %% option from Bn to An+1 using B + X
    OptB1 = {DistB + B, [{b,B}|PathB]}, %% option from Bn to Bn+1 using B
    OptB2 = {DistA + A + X, [{x,X}, {a,A}|PathA]}, %% option from An to Bn+1 using A + X
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

optimal_path(Map) ->
    {A,B} = lists:foldl(fun shortest_step/2, {{0,[]}, {0,[]}}, Map),
    {_Dist,Path} = if hd(element(2,A)) =/= {x,0} -> A;
                      hd(element(2,B)) =/= {x,0} -> B
                   end,
    lists:reverse(Path).

road_test() ->
    _ = group_vals([50,10,30,5,90,20,40,2,25,10,8,0],[]), %% [{50,10,30},{5,90,20},{40,2,25},{10,8,0}]
    _ = parse_map("50 10 30 5 90 20 40 2 25 10 8 0"), %% [{50,10,30},{5,90,20},{40,2,25},{10,8,0}]
    _ = parse_map(<<"50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0\n">>), %% [{50,10,30},{5,90,20},{40,2,25},{10,8,0}]
    _ = shortest_step({50,10,30},{{0,[]},{0,[]}}), %% {{40,[{x,30},{b,10}]},{10,[{b,10}]}}
    _ = shortest_step({5,90,20},{{40,[{x,30},{b,10}]},{10,[{b,10}]}}), %% {{45,[{a,5},{x,30},{b,10}]}, {65,[{x,20},{a,5},{x,30},{b,10}]}}
    _ = shortest_step({40,2,25},{{45,[{a,5},{x,30},{b,10}]}, {65,[{x,20},{a,5},{x,30},{b,10}]}}), %% {{85,[{a,40},{a,5},{x,30},{b,10}]}, {67,[{b,2},{x,20},{a,5},{x,30},{b,10}]}}
    _ = shortest_step({10,8,0},{{85,[{a,40},{a,5},{x,30},{b,10}]}, {67,[{b,2},{x,20},{a,5},{x,30},{b,10}]}}), 
%% {{75,
%% [{x,0},{b,8},{b,2},{x,20},{a,5},{x,30},{b,10}]}, 
%% {75,
%% [{b,8},{b,2},{x,20},{a,5},{x,30},{b,10}]}}
    _ = element(2,{75, [{b,8},{b,2},{x,20},{a,5},{x,30},{b,10}]}), %% [{b,8},{b,2},{x,20},{a,5},{x,30},{b,10}]
    _ = hd(element(2,{75, [{b,8},{b,2},{x,20},{a,5},{x,30},{b,10}]})), %% {b,8}
    R = optimal_path([{50,10,30},{5,90,20},{40,2,25},{10,8,0}]), %% [{b,10},{x,30},{a,5},{x,20},{b,2},{b,8}]
    R.


