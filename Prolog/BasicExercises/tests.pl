:- consult(basics).

:- begin_tests(basics).

test(is_even) :-
  is_even(2), \+ is_even(3), is_even(120).

test(factorials) :-
  my_factorial(0, 1),
  my_factorial(1, 1),
  my_factorial(2, 2),
  my_factorial(3, 6),
  my_factorial(4, 24),
  my_factorial(5, 120),
  my_factorial(6, 720).

test(not_factorials) :-
  \+ my_factorial(-1, 1),
  \+ my_factorial(0, 2),
  \+ my_factorial(1, 2),
  \+ my_factorial(2, 3),
  \+ my_factorial(3, 7),
  \+ my_factorial(4, 25),
  \+ my_factorial(5, 121),
  \+ my_factorial(6, 721).

test(primes) :-
  is_prime(1),
  is_prime(2),
  is_prime(3),
  is_prime(5),
  is_prime(7),
  is_prime(443),
  is_prime(449),
  is_prime(457).

test(not_primes) :-
  \+ is_prime(10),
  \+ is_prime(20),
  \+ is_prime(49).

% Should take a list and split the elements into evens and odds.
test(correctly_split) :-
  split_evens_odds([], [], []),
  split_evens_odds([1], [], [1]),
  split_evens_odds([1,2], [2], [1]),
  split_evens_odds([1, 2, 3], [2], [1, 3]),
  split_evens_odds([1, 2, 3, 4], [2, 4], [1, 3]),
  split_evens_odds([7, 2, 3, 5, 8], [2, 8], [7, 3, 5]),
  split_evens_odds([8, 7, 6, 5, 4, 3], [8, 6, 4], [7, 5, 3]).

test(incorrectly_split) :-
  \+ split_evens_odds([], [2], [1]),
  \+ split_evens_odds([1], [], []),
  \+ split_evens_odds([1,2], [], [1]),
  \+ split_evens_odds([1, 2, 3], [2], [3]),
  \+ split_evens_odds([1, 2, 3, 4], [2], [1, 3]),
  \+ split_evens_odds([7, 2, 3, 5, 8], [8], [7, 3, 5]),
  \+ split_evens_odds([8, 7, 6, 5, 4, 3], [8, 4], [7, 5, 3]).

test(reversed) :-
  my_reverse([], []),
  my_reverse([1], [1]),
  my_reverse([1, 2], [2, 1]),
  my_reverse(['a', 'b'], ['b', 'a']),
  my_reverse([1, 2, 3], [3, 2, 1]),
  my_reverse([1, 2, 3, 4], [4, 3, 2, 1]).

test(not_reversed) :-
  \+ my_reverse([1, 2], [1, 2]),
  \+ my_reverse(['a', 'b'], ['a', 'b']),
  \+ my_reverse([1, 2, 3], [1, 2, 3]),
  \+ my_reverse([1, 2, 3, 4], [1, 2, 3, 4]).

% Takes a list of numbers and returns the product of each one.
test(correct_prod_list) :-
  product_list([], 0),
  product_list([4, 3], 12),
  product_list([6, 2, 5, 10], 600),
  product_list([-1, 10], -10).

test(incorrect_prod_list) :-
  \+ product_list([], 1),
  \+ product_list([4, 3], 0),
  \+ product_list([6, 2, 5, 10], 700),
  \+ product_list([-1, 10], -11).

% Returns true if the given prefix and suffix match the given list.
test(has_bookends) :-
  has_bookends([], [], []),
  has_bookends(['a'], ['c'], ['a', 'b', 'c']),
  has_bookends([1], [3, 4, 5], [1, 2, 3, 4, 5]),
  has_bookends([], [4], [1, 2, 3, 4]),
  has_bookends([1, 2, 3], [3, 4], [1, 2, 3, 4]),

test(does_not_have_bookends) :-
  \+ has_bookends([5], [2, 3], [1, 2, 3, 4]).
  \+ has_bookends(['d'], ['c'], ['a', 'b', 'c']),
  \+ has_bookends([1], [3, 4, 5], [1, 2, 3, 4, 5]),
  \+ has_bookends([7], [4], [1, 2, 3, 4]),
  \+ has_bookends([1, 3], [3, 4], [1, 2, 3, 4]),

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

