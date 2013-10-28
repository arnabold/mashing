-module(dolphins).
-compile(export_all).

%% Receiving messages

dolphin1() ->
    receive
        do_a_flip ->
            io:format("How about no?~n");
        fish ->
            io:format("So long and thanks for all the fish!~n");
        _ ->
            io:format("Heh, we're smarter than you humans.~n")
    end.

%% Packaging a process' pid in a tuple. The end result is a message that looks a bit like 
%% {Pid, Message}
%% reply a message to the sender
dolphin2() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?";
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
        _ ->
            io:format("Heh, we're smarter than you humans.~n")
    end.

%% We just need the function to call itself so it never ends and always expects more messages.
dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
        _ ->
            io:format("Heh, we're smarter than you humans.~n"),
            dolphin3()
    end.

main() ->
    Dolphin = spawn(dolphins, dolphin1, []), %% <0.267.0>
    Dolphin ! "oh, hello dolphin!", %% Heh, we're smarter than you humans. "oh, hello dolphin!"
    Dolphin ! fish, %% fish %% Here the process is already terminated
    %% We'll need to restart the dolphin:
    %% only in shell f(Dolphin),
    DolphinBis = spawn(dolphins, dolphin1, []),
    DolphinBis ! fish, %% So long and thanks for all the fish! %% fish

    Dolphin2 = spawn(dolphins, dolphin2, []),
    Dolphin2 ! {self(), do_a_flip}, %% {<0.296.0>,do_a_flip} %% send a message to dolphin2
    c:flush(), %% Shell got "How about no?" %% read the messages in mailbox

    Dolphin3 = spawn(dolphins, dolphin3, []),
    Dolphin3 ! Dolphin3 ! {self(), do_a_flip}, %% {<0.296.0>,do_a_flip}
    c:flush(), %% Shell got "How about no?" %% Shell got "How about no?"
    Dolphin3 ! {self(), unknown_message}, %% Heh, we're smarter than you humans. %% {<0.296.0>,unknown_message}
    Dolphin3 ! Dolphin3 ! {self(), fish}, %% {<0.296.0>,fish}
    c:flush(). %% Shell got "So long and thanks for all the fish!"
