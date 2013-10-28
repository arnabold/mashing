-module(tree_ex).
-export([empty/0, insert/3, lookup/2, has_value/2, test/0]).

empty() -> {node, 'nil'}.

insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
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


% looks for a given value 'Val' in the tree.
has_value_bad(_, {node, 'nil'}) ->
    false;
has_value_bad(Val, {node, {_, Val, _, _}}) ->
    true;
has_value_bad(Val, {node, {_, _, Left, Right}}) ->
    case has_value(Val, Left) of %% look the Left barnch before
        true -> true;
        false -> has_value_bad(Val, Right) %% if false llok for the Right branch
    end.
%% The problem with this implementation is that every node of the tree we branch at has to test for the result of the 
%% previous branch


has_value(Val, Tree) -> 
    try has_value1(Val, Tree) of
        _ -> false
    catch
        true -> true
    end.

has_value1(_, {node, 'nil'}) ->
    false;
has_value1(Val, {node, {_, Val, _, _}}) ->
    throw(true);
has_value1(Val, {node, {_, _, Left, Right}}) ->
    has_value1(Val, Left), 
    has_value1(Val, Right).

test() ->
    N0 = insert("Antonella", 1, empty()),
    N1 = insert("Fabio", 2, N0),
    N2 = insert("Francesco", 3, N1),
    N3 = insert("Edoardo", 4, N2),
    Tree = insert("Alessandro", 5, N3),
    _ = lookup("Francesco", Tree), %% 3
    _ = lookup("Silvia", Tree), %% undefined
    _ = has_value_bad(1, empty()), %% false
    _ = has_value_bad(3, Tree), %% true
    _ = has_value_bad(10, Tree), %% false
    _ = has_value(1, empty()), %% false
    _ = has_value(3, Tree), %% true
    R = has_value(10, Tree), %% false
    R.
%% Tree is
%% {node,{"Antonella",1,
%%       {node,{"Alessandro",5,{node,nil},{node,nil}}},
%%       {node,{"Fabio",2,
%%              {node,{"Edoardo",4,{node,nil},{node,nil}}},
%%              {node,{"Francesco",3,{node,nil},{node,nil}}}}}}}
