-module(modules).

-export([modules/0, add/2, hello/0, greet_and_add_two/1]).

%% macro
-define(sub(X,Y), X-Y).
 
modules() ->
    %% erlang module functions are automatically imported
    _ = erlang:element(2, {a,b,c}), %% b
    _ = element(2, {a,b,c}), %% b
    lists:seq(1,4), %% [1,2,3,4]
    _ = ?sub(23,47),
    0.

add(A,B) ->
    A + B.

hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).
 

%% Change current directory
%% cd("/path/to/where/you/saved/the-module/").

%% compile module:
%% c(modules).
%% c(useless, [debug_info, export_all])

%% -compile([debug_info, export_all]).

%% compile in native code
%% hipe:c(Module,OptionsList)

%% modules:module_info().
