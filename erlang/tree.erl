-module(tree).
-export([empty/0, insert/3, lookup/2]).


%% A tree.
%% Nodes are tuples that contain a key, a value associated to the key, and then two other nodes. 
%% Of these two nodes, we need one that has a smaller and one that has a larger key than the node holding them. 
%% A tree is a node containing nodes. 
%% To represent nodes, tuples are an appropriate data structure. 
%% we can then define these tuples as {node, {Key, Value, Smaller, Larger}} (a tagged tuple!), where Smaller and Larger
%% can be another similar node or an empty node ({node, nil}). 
 
%% an empty node
empty() -> {node, 'nil'}.

%% the root is the starting point of a tree

%% insert in an empty node
insert(Key,Val,{node,'nil'}) ->
    {node,{Key,Val,{node,'nil'},{node,'nil'}}}; %% a node and two empty nodes
%% insert in Smaller
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
%% insert in Larger
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
%% when value's key are equals, replace the node
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.

lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).


