-module(tut2).
-export([convert/2,test/0]).

%% Atoms

convert(M,inch) ->
    M / 2.54;
convert(N,centimeter) ->
    N * 2.54.

test() ->
    1.1811023622047243 = convert(3,inch),
    17.78 = convert(7,centimeter),
    convert(3, miles), %% ** exception error: no function clause matching tut2:convert(3,miles) (tut2.erl, line 4)
    ok.

%% in erlang shell v/1 to see the error tuple
%% 42> v(41).
%% {'EXIT',{function_clause,[{tut2,convert,
%%                                 [3,miles],
%%                                 [{file,"tut2.erl"},{line,4}]},
%%                           {tut2,test,0,[{file,"tut2.erl"},{line,12}]},
%%                           {erl_eval,do_apply,6,
%%                                     [{file,"erl_eval.erl"},{line,576}]},
%%                           {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
%%                           {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
%%                           {shell,eval_loop,3,
%%                                  [{file,"shell.erl"},{line,608}]}]}}

%% Problem: Does this mean that 3 is in inches? or that 3 is in centimeters and we want to convert it to inches? So Erlang has a way to group things together to make things more understandable. We call these tuples. 

