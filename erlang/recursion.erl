-module(recursion).
-compile(export_all).

%% Hello recursion!

%% factorial
%% recursion:fac(0). %% 1
%% recursion:fac(1). %% 1
%% recursion:fac(3). %% 6
fac (0) ->
    1;
fac (N) when N > 0 ->
    N * fac(N-1).

%% length
%% a list like [a] could be written as [a|[]]
len([]) ->
    0;
len([_|T]) ->
    1 + len(T).

%% Tail Recursion

tail_fac(N) ->
    tail_fac(N,1).

tail_fac(0, Acc) ->
    Acc;
tail_fac(N, Acc) when N > 0 ->
    tail_fac(N-1, N*Acc).

tail_len(List) -> 
    tail_len(List,0).

tail_len([],Acc) ->
    Acc;
tail_len([_|Tail], Acc) ->
    tail_len(Tail, Acc+1).

%% More recursive functions

%% duplicate/2
%% integer n, term -> a list of n terms

duplicate(0,_) ->
    [];
duplicate(N,Term) when N > 0 ->
    [Term | duplicate(N-1,Term)].

tail_duplicate(N,Term) ->
    tail_duplicate(N,Term,[]).

tail_duplicate(0,_,List) ->
    List;
tail_duplicate(N,Term,List) when N > 0 ->
    tail_duplicate(N-1,Term,[Term|List]).

%% reverse/1

last([]) ->
    [];
last([Term]) ->
    Term;
last([_|Tail]) ->
    last(Tail).

firsts([]) ->
    [];
firsts([_]) ->
    [];
firsts([Head|Tail]) ->
    [Head|firsts(Tail)].

my_reverse([]) ->
    [];
my_reverse(List) ->
    [last(List)|my_reverse(firsts(List))].

reverse([]) ->
    [];
reverse([Head|Tail]) ->
    reverse(Tail) ++ [Head].

tail_reverse(List) ->
    tail_reverse(List,[]).

tail_reverse([],Acc) ->
    Acc;
tail_reverse([Head|Tail],Acc) ->
    tail_reverse(Tail,[Head|Acc]).

%% sublist/2
%% takes a list L and an integer N, and returns the N first elements of the list.
%% sublist([1,2,3,4,5,6],3) would return [1,2,3]

sublist(_,0) ->
    [];
sublist([],_) ->
    [];
sublist([Head|Tail],N) when N > 0 ->
    [Head|sublist(Tail,N-1)].

tail_sublist(List,N) ->
    tail_sublist(List,N,[]).

tail_sublist([],_,Acc) ->
    Acc;
tail_sublist(_,0,Acc) ->
    Acc;
tail_sublist([Head|Tail],N,Acc) when N > 0 ->
    tail_reverse(tail_sublist(Tail,N-1,[Head|Acc])).

%% instead of writing your own reverse/1 function, you should use lists:reverse/1.

%% zip/2
%% will take two lists of same length as parameters and will join them as a list of tuples which all hold two terms.
%% recursion:zip([a,b,c],[1,2,3]). returns [{a,1},{b,2},{c,3}]

zip([],[]) ->
    [];
zip([X|Xs],[Y|Ys]) ->
    [{X,Y}|zip(Xs,Ys)].

lenient_zip(_,[]) ->
    [];
lenient_zip([],_) ->
    [];
lenient_zip([X|Xs],[Y|Ys]) ->
    [{X,Y}|lenient_zip(Xs,Ys)].

tail_zip(List1,List2) ->
    tail_reverse(tail_zip(List1,List2,[])).
tail_zip([],[],Acc) ->
    Acc;
tail_zip([X|Xs],[Y|Ys],Acc) ->
    tail_zip(Xs,Ys,[{X,Y}|Acc]).

tail_lenient_zip(List1,List2) ->
    tail_reverse(tail_lenient_zip(List1,List2,[])).
tail_lenient_zip(_,[],Acc) ->
    Acc;
tail_lenient_zip([],_,Acc) ->
    Acc;
tail_lenient_zip([X|Xs],[Y|Ys],Acc) ->
    tail_lenient_zip(Xs,Ys,[{X,Y}|Acc]).

%% Quicksort
%% A naive implementation of quicksort works by taking the first element of a list, the pivot, and then putting all 
%% the elements smaller or equal to the pivot in a new list, and all those larger in another list. We then take 
%% each of these lists and do the same thing on them until each list gets smaller and smaller. This goes on until 
%% you have nothing but an empty list to sort, which will be our base case. This implementation is said to be naive 
%% because smarter versions of quicksort will try to pick optimal pivots to be faster. We don't really care about 
%% that for our example though.

quicksort([]) ->
    [];
quicksort([Pivot|Rest]) ->
    {Smaller,Larger} = partition(Pivot,Rest,[],[]),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_,[],Smaller,Larger) ->
    {Smaller,Larger};
partition(Pivot,[Head|Tail],Smaller,Larger) ->
    if Head =< Pivot ->
            partition(Pivot,Tail,[Head|Smaller],Larger);
       Head > Pivot ->
            partition(Pivot,Tail,Smaller,[Head|Larger])
    end.

lc_quicksort([]) ->
    [];
lc_quicksort([Pivot|Rest]) ->
    lc_quicksort([E || E <- Rest, E =< Pivot]) 
        ++ [Pivot] ++ 
        lc_quicksort([E || E <- Rest, E > Pivot]).

%% More than lists

















            











