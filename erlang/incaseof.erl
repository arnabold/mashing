-module(incaseof).
-compile(export_all).

%% incaseof:insert(a,[]). %% [a]
%% incaseof:insert(a,[a,b]). %% [a,b]
%% incaseof:insert(a,[b,c]). %% [a,b,c]
insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
        true  -> Set;
        false -> [X|Set]
    end.

%% incaseof:beach({celsius, 30}). %% favorable
%% incaseof:beach({kelvin, 300}). %% 'scientifically favorable'
%% incaseof:beach({fahrenheit, 100}). %% 'favorable in the US'
%% incaseof:beach({xxx, 100}). %% 'avoid beach'
beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.
