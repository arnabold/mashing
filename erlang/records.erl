-module(records).
-compile(export_all).

-record(robot, {name, type=industrial, hobbies, details=[]}).

%% Erlang records are just syntactic sugar on top of tuples.
%% The Erlang shell has a command rr(Module) that lets you load record definitions from Module

first_robot() ->
    #robot{name="Mechatron", type=handmade, details=["Moved by small man inside"]}.

car_factory(CorpName) ->
    #robot{name=CorpName, hobbies="building cars"}.

-record(user, {id, name, group, age}).

%% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
    %% Show stuff that can't be written in such a text
    allowed;
adult_section(_) ->
    %% redirect to sesame street site
    forbidden.

repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.

-include("records.hrl"). %% included record definition
included() -> 
    #included{some_field="Some value"}.

records_spike() ->
    _ = first_robot(), %% {robot,"Mechatron",handmade,undefined,["Moved by small man inside"]}
    %% rr(records)
    %% #robot{name = "Mechatron",type = handmade,hobbies = undefined,details = ["Moved by small man inside"]}
    _ = car_factory("Jokeswagen"), %% #robot{name = "Jokeswagen",type = industrial, hobbies = "building cars",details = []}
    %% on rr() can take more than a module name: it can take a wildcard (like rr("*")) and also a list as a second argument to specify which records to load.
    %% rd(Name, Definition) lets you define a record in a manner similar to the -record(Name, Definition) used in our module. 
    %% You can use rf() to 'unload' all records, or rf(Name) or rf([Names]) to get rid of specific definitions.
    %% You can use rl() to print all record definitions in a way you could copy-paste into the module or use rl(Name) or rl([Names]) to restrict it to specific records.
    %% rp(Term) lets you convert a tuple to a record (given the definition exists).
    Crusher = #robot{name="Crusher",hobbies=["Crushing people","petting cats"]},
    _ = Crusher#robot.hobbies, %% ["Crushing people","petting cats"]
    NestedBot = #robot{details=#robot{name="erNest"}}, 
    _ = NestedBot#robot.details#robot.name, %% "erNest"
    _ = #robot.type, %% 3. What this outputs is which element of the underlying tuple it is.
    _ = admin_panel(#user{id=1, name="ferd", group=admin, age=96}), %% "ferd is allowed!"
    _ = admin_panel(#user{id=2, name="you", group=users, age=66}), %% "you is not allowed"
    _ = adult_section(#user{id=21, name="Bill", group=users, age=72}), %% allowed
    _ = adult_section(#user{id=22, name="Noah", group=users, age=13}), %% forbidden
    _ = repairman(#robot{name="Ulbert", hobbies=["trying to have feelings"]}),
    R = included(), %% {included,"Some value","yeah!",undefined}
    R.

