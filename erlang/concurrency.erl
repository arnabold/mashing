-module(concurrency).
-compile(export_all).

%% A process runs a function, and once it’s finished, it disappears
%% processes do not return the result of the running fumction
%% spawn returns a process identifier
spawning_processes() ->
    F = fun() -> 2 + 2 end, %% #Fun<erl_eval.20.82930912>
    _ = spawn(F), %% process identifier or pid. i.e.: <0.164.0> 
    Pid = spawn(fun() -> io:format("~p~n",[2 + 2]) end), %% should print 4
    Pid.

%% the processes execution order doesn't make sense because of parallelism
parallel_processes() ->
    %% G a function that sleeps for 10 milliseconds and then print its argument
    G = fun(X) -> timer:sleep(10), io:format("~p~n", [X]) end,
    [spawn(fun() -> G(X) end) || X <- lists:seq(1,10)]. %% [<0.169.0>,<0.170.0>,<0.171.0>,<0.172.0>,<0.173.0>,<0.174.0>,<0.175.0>,<0.176.0>,<0.177.0>,<0.178.0>].


self_test() ->
    %% self returns the pid of the current process
    %% erlang shell is one of those processes
    io:format("The pid of the current process is ~p~n.", [self()]),
    %%_ = exit(self()), %% exception exit: <0.217.0>    
    %% <0.237.0> %% the pid changes because the process has been restarted
    io:format("The pid of the current process is ~p~n.", [self()]).

sending_messages() ->
    %% send message hello to process self()
    _ = self() ! hello, % hello %% bang operatot: put the meassge in the process queue
    R = self() ! self() ! double, %% send double message to process two times. 
    %% Which is equivalent to self() ! (self() ! double).    
    %% the messages are kept in the order they are received.
    c:flush(), %% Shell got hello Shell got double Shell got double ok 
    %% To see the contents of the current mailbox
    R. %% bang return the message sent

