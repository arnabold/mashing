-module(multiproc).
-compile(export_all).

%% how to implement a sleep
sleep(T) ->
    receive %% no message will ever be matched in the receive part of the construct 
            %% because there is no pattern
    after T ->
            ok
    end.

sleep_test() ->
    sleep(3000). %% ok after 3 secs.

%% As long as there are messages, the flush/0 function will recursively call itself 
%% until the mailbox is empty. 
%% Once this is done, the after 0 -> ok part of the code is executed and the function returns.
flush() ->
    receive
        _ -> io:format("."),
             flush()
    after 0 ->
            ok
    end.

flush_test() ->
    self() ! self() ! self() ! msg1,
    flush().

%% Selective Receives

%% function to receive messages with priority > 0 and the switch to normal
important() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message | important()]
    after 0 ->
        normal()
    end.

%% function to receive other messages and when the mailbox is empty returns []
normal() ->
    receive
        {_, Message} ->
            [Message | normal()]
    after 0 ->
        []
    end.

selective_test() ->
    self() ! {15, high}, 
    self() ! {7, low}, 
    self() ! {1, low}, 
    self() ! {17, high}, %% {15,high}
    multiproc:important(). % [high,high,low,low]

