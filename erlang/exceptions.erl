-module(exceptions).
-compile(export_all).

%% exceptions:throws(fun() -> throw(thrown) end). %% {throw,caught,thrown}
%% exceptions:throws(fun() -> erlang:error(pang) end).
%% ** exception error: pang
%%     in function  exceptions:throws/1 (exceptions.erl, line 5)
throws(F) ->
    try F() of %% expression to be protected
        _ -> ok %% successful pattern -> expression
    catch
        Throw -> {throw, caught, Throw} %% type_of_error -> expression
    end.

%% exceptions:errors(fun() -> erlang:error("Die!") end).
%% {error,caught,"Die!"} 
errors(F) ->
    try F() of
        _ -> ok
    catch
        error:Error -> {error, caught, Error}
    end.

%% exceptions:exits(fun() -> exit(goodbye) end).
%% {exit,caught,goodbye}
exits(F) ->
    try F() of
        _ -> ok
    catch
        exit:Exit -> {exit, caught, Exit}
    end.                         
        
%% functions to generate various kind of exceptions    
sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).
talk() -> "blah blah".
    
%% exceptions:talk(). %% "blah blah"
%% exceptions:black_knight(fun exceptions:talk/0). %% "None shall pass."
%% exceptions:sword(1). %% ** exception throw: slice
%% exceptions:black_knight(fun() -> exceptions:sword(1) end). %% "It is but a scratch."
%% exceptions:sword(2). %% ** exception error: cut_arm
%% exceptions:black_knight(fun() -> exceptions:sword(2) end). %% "I've had worse."
%% exceptions:sword(3). %% ** exception exit: cut_leg
%% exceptions:black_knight(fun() -> exceptions:sword(3) end). %% "Come on you pansy!"
%% exceptions:sword(4). %% ** exception throw: punch
%% exceptions:black_knight(fun() -> exceptions:sword(4) end). %% "Just a flesh wound."
%% exceptions:sword(5). %% ** exception exit: cross_bridge
%% exceptions:black_knight(fun() -> exceptions:sword(5) end). %% "Just a flesh wound."
black_knight(Attack) when is_function(Attack, 0) -> %% Attack is a function with arity 0
    try Attack() of
        _ -> "None shall pass."
    catch
        throw:slice -> "It is but a scratch.";
        error:cut_arm -> "I've had worse.";
        exit:cut_leg -> "Come on you pansy!";
        _:_ -> "Just a flesh wound."
    end.

%% exceptions:whoa(). %% {caught,throw,up}
whoa() ->
    try
        talk(),
        _Knight = "None shall Pass!",
        _Doubles = [N*2 || N <- lists:seq(1,100)],
        throw(up), %% throw exception
        _WillReturnThis = tequila
    of
        tequila -> "hey this worked!"
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

%% without of
%% exceptions:im_impressed(). %% {caught,throw,up}
im_impressed() ->
    try
        talk(),
        _Knight = "None shall Pass!",
        _Doubles = [N*2 || N <- lists:seq(1,100)],
        throw(up),
        _WillReturnThis = tequila
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

%% catch
test_catch() ->
    catch throw(whoa), %% whoa
    %% catch exit(die), %% {'EXIT',die}
    %% catch 1/0, %% {'EXIT',{badarith,[{erlang,'/',[1,0],[]},                   
%% {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},                   
%% {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,360}]},                   
%% {shell,exprs,7,[{file,"shell.erl"},{line,668}]},                   
%% {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},                   
%% {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]}}
    catch 2 + 2, %% 4
    catch doesnt:exist(a,4), %% {'EXIT',{undef,[{doesnt,exist,[a,4],[]},
%% {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
%% {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,360}]},
%% {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
%% {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
%% {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]}}
    0.

%% exceptions:catcher(3,3). %% 1.0
%% exceptions:catcher(6,3). %% 2.0
%% exceptions:catcher(6,0). %% "uh oh"
catcher(X,Y) ->
    case catch X/Y of
        {'EXIT', {badarith,_}} -> "uh oh";
        N -> N
    end.

problems_with_catch() ->
    %% X = catch 4+2, %% syntax error before: 'catch'
    X = (catch 4+2), %% 6
    _ = X,
    catch erlang:boat(), %% {'EXIT',{undef,[{erlang,boat,[],[]},
%%                {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
%%                {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,360}]},
%%                {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
%%                {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
%%                {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]}}
    catch exit({undef, [{erlang,boat,[]}, 
                        {erl_eval,do_apply,5}, 
                        {erl_eval,expr,5}, 
                        {shell,exprs,6}, 
                        {shell,eval_exprs,6}, 
                        {shell,eval_loop,3}]}), %% {'EXIT',{undef,[{erlang,boat,[]},
%%                {erl_eval,do_apply,5},
%%                {erl_eval,expr,5},
%%                {shell,exprs,6},
%%                {shell,eval_exprs,6},
%%                {shell,eval_loop,3}]}}
    0.

one_or_two(1) -> return;
one_or_two(2) -> throw(return).

%% catch exceptions:one_or_two(1). %% return
%% catch exceptions:one_or_two(2). %% return

