% 7.1)
simplify(A, A) :- number(A), !.
simplify(A+N, A+N) :- atom(A), number(N), !.
simplify(N+A, A+N) :- atom(A), number(N), !.
simplify(X+A, E+Sum) :- number(A), !, simplify(X, E+N), Sum is N+A.
simplify(X+A, E+A+Sum) :- atom(A), simplify(X, E+Sum).

% 7.2)
add_to_tail(A, [H|T]) :- nonvar(H), !, add_to_tail(A, T).
add_to_tail(A, [A|NewTail]) :- nonvar(A).

% Fail goal is the one I couln't figure out. Negation *is* useful
member(A, L) :- var(L), !, fail.
member(A, [L]) :- var(L), !, fail. % check T isn't variable
member(A, [A|_]) :- nonvar(A), !.
member(A, [_|T]) :- member(A, T).

% 7.3)
grnd_list([]) :- !.
grnd_list([H|T]) :- grnd(H), !, grnd_list(T).

% Note how the colour of the cut depends on the mutual exclusivity of the clause. 
grnd(Term) :- atomic(Term), !.
grnd(Term) :- nonvar(Term), Term =.. List, grnd_list(List).
% RED CUT				   ^compound(Term),

% 7.4) Remove the cut for Case 1.

% 7.5) This should work. But so should bog-standard unification.
% What is the point of this exercise?
subsume_list([], []).
subsume_list([H|T], [A|B]) :- subsume(H,A), subsume_list(T,B).

subsume(X, Y) :- not(compound(X)), X=Y, !.
subsume(X, Y) :-
	X =.. X_List,				% green cut, no compound check
	compound(Y), Y =.. Y_List,
	subsume_list(X_List, Y_List).

% 7.6) Need fail to enforce backtracking.
% retract((product(_,_,_))), fail.
% retract((product(_,_,0))), fail.

% 7.7) After having looked at the answer, the trick is to encapsulate the term
% in a single argument functor that makes it so assert and retract can only refer
% to the same term.
cp_trm(Term, Copy) :-
	asserta(term_to_cp(Term)),
	retract(term_to_cp(Copy)).

% 7.8) Assume no duplicates.
subs([], []).
subs([H|T], [H|L]) :- subs(T, L).
subs(T, [_|L]) :- subs(T, L).

powerset(Set, Subsets) :- bagof(S, subs(S, Set), Subsets).

% 7.9) They had 
% copy_term(Term, Copy) :- bagof(X, X=Term, [Copy]).
cp_tm2(Term, Copy) :- bagof(Term, true, [Copy]).
