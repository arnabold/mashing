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
    'Blanks can be quoted',
    'Anything inside quotes \n\012'.

test() ->
    integers(),
    floats(),
    atoms().
