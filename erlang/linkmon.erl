-module(linkmon).
-compile(export_all).

myproc() ->
    timer:sleep(5000),
    io:format("."),
    exit(reason).

%% link/1 will create a link between the current process and the one identified by Pid
%% so when Pid dies unexpected the current process also dies.
test1() ->
    %myproc(), %% exception exit: reason after 5 sec.
    spawn(fun myproc/0), %% pid
    link(spawn(fun myproc/0)). %% true and exception error: reason after 5 sec

chain(0) ->
    receive
        _ ->  ok
    after 2000 ->
            io:format("~w",[0]),
            exit("chian dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    io:format("~w",[N]),
    receive
        _ ->  ok
    end.

%% After the process running linkmon:chain(0) dies, the error is propagated down the chain of 
%% links until the shell process itself dies because of it. 
test2() ->
    link(spawn(linkmon, chain, [20])).

%% to kill another process
test3() ->
    Pid = spawn(fun linkmon:myproc/0),
    exit(Pid,reason).

%% System processes are basically normal processes, except they can convert exit signals 
%% to regular messages
test4() ->
    process_flag(trap_exit,true), %% true
    spawn_link(fun() -> linkmon:chain(3) end), %% <0.667.0> %% 3210
    receive X -> X end. %% {'EXIT',<0.667.0>,"chian dies here"}

test5() ->
    process_flag(trap_exit,false), 
    spawn_link(fun() -> ok end), %% - nothing -
    %% spawn_link(fun() -> exit(reason) end), %% exception exit: reason
    spawn_link(fun() -> exit(normal) end), %% - nothing -
    %% spawn_link(fun() -> 1/0 end), 
    %% Error in process <X.Y.Z> with exit value: {badarith, Reason}
    %% spawn_link(fun() -> erlang:error(reason) end), 
    %% Error in process <X.Y.Z> with exit value: {reason,Reason}
    %% spawn_link(fun() -> throw(rocks) end), 
    %% Error in process <0.899.0> with exit value: {{nocatch,rocks}, Reason }
    %% exit/2
    %% exit(self(), normal), %% exception exit: normal
    ok.

test6() ->
    process_flag(trap_exit,true), 
    spawn_link(fun() -> ok end), %% {EXIT, <X.Y.Z>, normal}
    spawn_link(fun() -> exit(reason) end), %% {EXIT, <X.Y.Z>, reason}    
    spawn_link(fun() -> exit(normal) end), %% {EXIT, <X.Y.Z>, normal}
    spawn_link(fun() -> 1/0 end), %% {'EXIT',<X.Y.Z>,{badarith,Reason}}
    spawn_link(fun() -> erlang:error(reason) end), %% {'EXIT',<X.Y.Z>,{reason,Reason}}
    spawn_link(fun() -> throw(rocks) end), %% {'EXIT',<0.914.0>, {{nocatch,rocks},Reason}}
    %% exit/2
    exit(self(), normal), %% ???

    c:flush(). 

