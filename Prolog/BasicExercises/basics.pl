% rich(colMustard) is a single fact that can be added or removed
% which changes the suspect list, so I am setting the rich facts
% to dynamic so it can be retracted for testing purposes.
:- dynamic(rich/1).

is_even(N) :- is_divisible(N, 2).

% defined for whole numbers (e.g. {0, 1, 2, ...}).
% returns 1 for negative numbers for simplicity sake and to avoid crashing.
my_factorial(X, 1) :- X < 0, !.
my_factorial(X, Result) :- my_factorial(X, 1, Result).

my_factorial(0, Result, Result) :- !.
my_factorial(X, Acc, Result) :-
  NewX is X - 1,
  NewAcc is Acc * X,
  my_factorial(NewX, NewAcc, Result).

% 1 is prime here for simplicity
is_prime(1) :- !.
is_prime(2) :- !.
is_prime(SomeInteger) :- is_prime(SomeInteger, 2).

is_prime(X, Y) :- X < Y * Y, !.
is_prime(X, Y) :-
  \+ is_divisible(X, Y),
  NewY = Y + 1,
  is_prime(X, NewY).

is_divisible(X, Y) :- 0 =:= mod(X, Y).

split_evens_odds(List, Evens, Odds) :-
  split_evens_odds(List, [], [], Evens, Odds).

split_evens_odds([], EvenAcc, OddAcc, Evens, Odds) :-
  my_reverse(EvenAcc, Evens),
  my_reverse(OddAcc, Odds).
split_evens_odds([Head | Tail], EvenAcc, OddAcc, Evens, Odds) :-
  (is_even(Head) ->
     split_evens_odds(Tail, [Head | EvenAcc], OddAcc, Evens, Odds)
  ;  split_evens_odds(Tail, EvenAcc, [Head | OddAcc], Evens, Odds)).

my_reverse(List, Result) :- my_reverse(List, [], Result).

my_reverse([], Result, Result).
my_reverse([Head | Tail], Acc, _) :-
  my_reverse(Tail, [Head | Acc], _).

% returns the product of each number in the given list.
product_list([], 0) :- !.
product_list(List, Product) :- product_list(List, 1, Product).

product_list([], Product, Product).
product_list([Head | Tail], Acc, _) :-
  NewAcc is Acc * Head,
  product_list(Tail, NewAcc, _).

% returns true if the given prefix and suffix is a
% prefix and suffix of the given list.
has_bookends(Prefix, Suffix, List) :-
  is_prefix(Prefix, List),
  is_suffix(Suffix, List).

is_prefix([], _).
is_prefix([PrefixHead | PrefixTail], [ListHead | ListTail]) :-
  PrefixHead == ListHead,
  is_prefix(PrefixTail, ListTail).

is_suffix(Suffix, List) :-
  length(Suffix, SuffixLen), length(List, ListLen),
  ListLen >= SuffixLen,
  NumToDrop is ListLen - SuffixLen,
  drop(NumToDrop, List, Remaining),
  Suffix == Remaining.

% drops the given number of elements from a list starting at the head.
drop(0, Result, Result) :- !.
drop(NumToDrop, List, []) :- length(List, Len), NumToDrop > Len, !.
drop(NumToDrop,[_ | Tail] , Result) :-
  NumToDrop > 0,
  NewNumToDrop is NumToDrop - 1,
  drop(NewNumToDrop, Tail, Result).

has_sublist([], _) :- !.
has_sublist(SubList, List) :-
  [Head | _] = SubList,
  drop_until_elem(Head, List, Remaining),
  (is_prefix(SubList, Remaining) -> true
  ; [_ | Tail] = Remaining,
     has_sublist(SubList, Tail)).

% drops elements from a list starting at the head until a given element is reached.
% drops the entire list if element is not found.
drop_until_elem(_ , [], []) :- !.
drop_until_elem(E, [Head | Result], [Head | Result]) :- E == Head, !.
drop_until_elem(E, [_ | Tail], Result) :-
  drop_until_elem(E, Tail, Result).

married(profPlum, msGreen).
married(msGreen, profPlum).

having_affair(mrBoddy, msGreen).
having_affair(msGreen, myBoddy).
having_affair(mrBoddy, missScarlet).
having_affair(missScarlet, mrBoddy).

rich(mrBoddy).
% The single fact added for to narrow the suspects.
rich(colMustard).

greedy(colMustard).

hates(Hater, Hated) :-
  married(Hater, HatersSpouse),
  having_affair(Hated, HatersSpouse), !.

will_murder_for_money(Person, Victim) :-
  greedy(Person),
  \+ rich(Person),
  rich(Victim).

suspect(Killer, Victim) :-
  hates(Killer, Victim);
  will_murder_for_money(Killer, Victim).
