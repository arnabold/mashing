-module(tut12).
-export([month_length/2,test/0]).

%% Built In Functions (BIFs)

month_length(Year, Month) ->
    %% All years divisible by 400 are leap
    %% Years divisible by 100 are not leap (except the 400 rule above)
    %% Years divisible by 4 are leap (except the 100 rule above)
    Leap = if
        Year rem 400 == 0 ->
            leap;
        Year rem 100 == 0 ->
            not_leap;
        Year rem 4 == 0 ->
            leap;
        true ->
            not_leap
    end,  
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end.

test() ->
    29 = month_length(2004, feb),
    28 = month_length(2003, feb),
    31 = month_length(1947, aug),
    5 = trunc(5.6),
    6 = round(5.6),
    4 = length([a,b,c,d]),
    5.0 = float(5),
    true = is_atom(hello),
    false = is_atom("hello"),
    true = is_tuple({paris, {c, 30}}),
    false = is_tuple([paris, {c, 30}]),
    "hello" = atom_to_list(hello),
    goodbye = list_to_atom("goodbye"),
    "22" = integer_to_list(22),
    ok.

