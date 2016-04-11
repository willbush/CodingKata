:- consult(basics).

:- begin_tests(basics).

test(is_even) :-
    is_even(2), \+ is_even(3), is_even(120).

test(my_factorial) :-
    my_factorial(-1, R0), R0 =:= 1,
    my_factorial(0, R1), R1 =:= 1,
    my_factorial(1, R2), R2 =:= 1,
    my_factorial(2, R3), R3 =:= 2,
    my_factorial(3, R4), R4 =:= 6,
    my_factorial(4, R5), R5 =:= 24,
    my_factorial(5, R6), R6 =:= 120,
    my_factorial(6, R7), R7 =:= 720.

test(is_prime) :-
    is_prime(1),
    is_prime(2),
    is_prime(3),
    is_prime(5),
    is_prime(7),
    is_prime(443),
    is_prime(449),
    is_prime(457).

test(is_not_prime) :-
    \+ is_prime(10),
    \+ is_prime(20),
    \+ is_prime(49).

% Should take a list and split the elements into evens and odds.
test(split_evens_odds) :-
    split_evens_odds([], [], []),
    split_evens_odds([1], [], [1]),
    split_evens_odds([1,2], [2], [1]),
    split_evens_odds([1, 2, 3], [2], [1, 3]),
    split_evens_odds([1, 2, 3, 4], [2, 4], [1, 3]),
    split_evens_odds([7, 2, 3, 5, 8], [2, 8], [7, 3, 5]),
    split_evens_odds([8, 7, 6, 5, 4, 3], [8, 6, 4], [7, 5, 3]).

test(my_reverse) :-
    my_reverse([], []),
    my_reverse([1], [1]),
    my_reverse([1, 2], [2, 1]),
    my_reverse(['a', 'b'], ['b', 'a']),
    my_reverse([1, 2, 3], [3, 2, 1]),
    my_reverse([1, 2, 3, 4], [4, 3, 2, 1]).

% Takes a list of numbers and returns the product of each one.
test(product_list) :-
    product_list([], 0),
    product_list([4, 3], 12),
    product_list([6, 2, 5, 10], 600),
    product_list([-1, 10], -10).

% Returns true if the given prefix and suffix match the given list.
test(has_bookends) :-
    has_bookends([], [], []),
    has_bookends(['a'], ['c'], ['a', 'b', 'c']),
    has_bookends([1], [3, 4, 5], [1, 2, 3, 4, 5]),
    has_bookends([], [4], [1, 2, 3, 4]),
    has_bookends([1, 2, 3], [3, 4], [1, 2, 3, 4]),
    \+ has_bookends([1], [2, 3], [1, 2, 3, 4]).

test(drop) :-
    drop(0, [], []),
    drop(1, [], []),
    drop(1, [1, 2], [2]),
    drop(2, [1, 2], []),
    drop(2, ['a', 'b', 'c'], ['c']),
    drop(3, [1, 2, 3, 4, 5, 6], [4, 5, 6]).

% Drops elements from the given list until the given element is reached.
% if element is not reached, then everything is dropped.
test(drop_until_elem) :-
    drop_until_elem(0, [], []),
    drop_until_elem(5, [1], []),
    drop_until_elem(1, [1], [1]),
    drop_until_elem(3, [1, 2, 3], [3]),
    drop_until_elem(1, [3, 2, 1, 2, 3, 4], [1, 2, 3, 4]),
    drop_until_elem('a', ['b', 'a', 'c'], ['a', 'c']).

% Returns true if the given sub-list is a sub-list of the given list.
test(has_sublist) :-
    has_sublist([], []),
    has_sublist([1], [1]),
    has_sublist([2, 3, 4], [1, 2, 3, 4, 5]),
    has_sublist(['b', 'c'], ['a', 'b', 'c', 'd']),
    has_sublist([], [1, 2, 3]),
    has_sublist([2, 1], [5, 4, 3, 2, 1]),
    has_sublist([1], [2, 1, 3]),
    \+ has_sublist([1, 2, 3], []).

test(suspect) :-
    findall(Killer, suspect(Killer, mrBoddy), [profPlum]).

test(suspect_with_modified_facts) :-
    findall(Killer, without_fact(rich(colMustard), suspect(Killer, mrBoddy)), Suspects),
    Suspects == [profPlum, colMustard].

:- end_tests(basics).

% check a predicate P if a given Fact wasn't true.
without_fact(Fact, P) :-
    setup_call_cleanup(asserta((Fact :- !, fail), Ref), P, erase(Ref)).

% This just halts after running the tests whether they pass or fail.
% I use this so I can run Guard on the unit test file without
% it entering the swipl REPL.
:- run_tests, halt(0) ; halt(0).

