-module(kitchen).
-compile(export_all).

%% no state
fridge1() ->
    receive
        {From, {store, _Food}} ->
            From ! {self(), ok},
            fridge1();
        {From, {take, _Food}} ->
            %% uh....
            From ! {self(), not_found},
            fridge1();
        terminate ->
            ok
    end.

fridge1_test() ->
    Pid = spawn(kitchen, fridge1, []),
    Pid ! {self(), {store, milk}},
    Pid ! {self(), {take, milk}},
    c:flush(). %% Shell got {<0.95.0>,ok} %% Shell got {<0.95.0>,not_found}

%% with state
fridge2(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok}, %% reply to From
            fridge2([Food|FoodList]); %% update state
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true -> %% Food in FoodList
                    From ! {self(), {ok, Food}}, %% reply to From with Food
                    fridge2(lists:delete(Food, FoodList)); %% delete Food from FoodList
                false -> %% Food not in FoodLIst
                    From ! {self(), not_found}, %% reply notfound to From
                    fridge2(FoodList)
            end;
        terminate ->
            ok
    end.

fridge2_test() ->
    Pid = spawn(kitchen, fridge2, [[baking_soda]]),
    Pid ! {self(), {store, milk}},
    Pid ! {self(), {store, bacon}},
    Pid ! {self(), {take, bacon}},
    Pid ! {self(), {take, turkey}},
    Pid ! terminate,
    c:flush(). %% Shell got {<0.62.0>,ok} Shell got {<0.62.0>,ok} Shell got {<0.62.0>,{ok,bacon}} Shell got {<0.62.0>,not_found}

%% send to Pid a message composed by sender pid, store atome and Food variable
%% then receive a message and returns the content
store(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

%% send to Pid a message with sender pid, take message and food
%% receive a message and returns the content 
take(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

store_take_test() ->
    Pid = spawn(kitchen, fridge2, [[baking_soda]]),
    _ = store(Pid, water), %% ok
    _ = take(Pid, water), %% {ok,water}
    _ = take(Pid, juice). %% not_found

%% ?MODULE is a macro returning the current module's name
start(FoodList) ->
    spawn(?MODULE, fridge2, [FoodList]).

start_test() ->
    Pid = start([rhubarb, dog, hotdog]),
    _ = take(Pid,dog), %% {ok,dog}
    _ = take(Pid,odg). %% not_found

%% Time Out

%% store2 with timeout
store2(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.

%% take2 with timeout
take2(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.

timeout_test() ->
    FakePid = c:pid(0,250,0),
    %% no process exists with this pid
    _ = take2(FakePid,dog). %% hang for 3 secs. because no process receiving the message
%% after taht returns timeout








