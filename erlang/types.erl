-module(types).
-compile(export_all).

%% Dynamite strong typing
%%Erlang is dynamically typed.
dynamite() ->
    %% 6 + "1", %% error: bad argument in an arithmetic expression
    0.

%% Type conversions
type_conversions() ->
    _ = list_to_integer("42"), %% 42
    _ = integer_to_list(42), %% "42"
    %% _ = list_to_integer("54.32"), %% error: bad argument
    _ = list_to_float("54.32"), %% 54.32
    _ = atom_to_list(true), %% "true"
    _ = list_to_bitstring("hi there"), %% <<"hi there">>
    _ = bitstring_to_list(<<"hi there">>), %% "hi there"
    _ = atom_to_binary(erlang, unicode), %% <<"erlang">> 
    _ = atom_to_list(erlang), %% "erlang" 
    _ = binary_to_atom(<<"erlang">>, unicode), %% erlang
    _ = binary_to_existing_atom(<<"erlang">>, unicode), %% erlang
    %% _ = binary_to_existing_atom(<<"no_existing_atom">>, unicode), %% error: bad argument
    _ = binary_to_list(<<"erlang">>), %% "erlang" 
    _ = bitstring_to_list(<<"erlang">>), %% "erlang"
    _ = binary_to_term(<<131,100,0,6,101,114,108,97,110,103>>), %% erlang
    _ = float_to_list(1.0), %% "1.00000000000000000000e+00"
    Foo = fun() -> 0 end, erlang:fun_to_list(Foo), %% "#Fun<erl_eval.20.82930912>"
    integer_to_list(42), %% "42" 
    integer_to_list(66, 16), %% "42"
    iolist_to_binary([<<1,2,3>>, [<<4,5>>,<<6>>], <<7,8>>]), %% <<1,2,3,4,5,6,7,8>> 
    list_to_atom("erlang"), %% erlang
    list_to_binary([<<1,2,3>>, [<<4,5>>,<<6>>], <<7,8>>]), %% <<1,2,3,4,5,6,7,8>> 
    list_to_bitstring([<<1,2,3>>, [<<4,5>>,<<6>>], <<7,8:4>>]), %% <<1,2,3,4,5,6,7,8:4>> 
    list_to_existing_atom("erlang"), %% erlang 
    list_to_float("1.0"), %% 1.0 
    list_to_integer("2A", 16), % 42
    list_to_pid("<0.4.1>"), %  <0.4.1> 
    list_to_tuple([erlang, "erlang", 1]), %% {erlang,"erlang",1} 
    pid_to_list(<0.4.1>), 
    pid_to_list(list_to_pid("<0.4.1>")), %% "<0.4.1>"
    %% port_to_list/1, %%ref_to_list/1, term_to_binary/1, term_to_binary/2 and tuple_to_list/1
    0.

%% To Guard a Datatype
guard_data_type() ->
    0.

