% I believe Church numerals in Prolog
prim(0, z) :- !.
prim(N, s(T1)) :- N > 0, N1 is N-1, prim(N1, T1).

% Assumes perfect primitive numbers
plus(A, z, A) :- !.
plus(A, s(B), C) :- plus(s(A), B, C).

% Assumes perfect primitive numbers
mult(A, z, z).
mult(A, s(z), A) :- !. % to allow for mult(X, ?, Z) but not recurse 4evs
mult(A, s(B), C) :- mult(A, B, C1), plus(A, C1, C).

% Tail call optimisation (10000000 is fine)
max_iter([X], X) :- !.
max_iter([H1,H2|T], X) :- H1 > H2, !, max_iter([H1|T], X).
max_iter([H1,H2|T], X) :- H2 > H1, !, max_iter([H2|T], X).

% Not so much, fails for 10000000
max([X], X) :- !.
max([H|T], H) :- max(T, X), H > X, !.
max([H|T], X) :- max(T, X), X > H.
% however, for efficiency/not re-evaluating max(T,X) twice
% max([H|T], Z) :- max(T, X), H > X, !, Z=H ; Z=X.

% Actually, as it turns out, this generate predicate can be written with
% and without Last Call optimisation :L
gen(0, [0]) :- !.
gen(N, [N|T]) :- N > 0, N1 is N-1, gen(N1, T).

% Optimised (reversed)
gen_iter(0, L, L) :- !.
gen_iter(N, SoFar, L) :- N > 0, N1 is N-1, gen_iter(N1, [N|SoFar], L).
gen_iter(N, L) :- gen_iter(N, [], L).
