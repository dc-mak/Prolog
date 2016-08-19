% Auxilliary predicates
writeList([]) :- !.
writeList([H|T]) :- write(H), nl, writeList(T).

range(0, L, L) :- !.
range(N, Sofar, L) :- N > 0, N1 is N-1, range(N1, [N|Sofar], L).
range(N, L) :- range(N, [], L).

% CLP
:- use_module(library(bounds)).
diff(L) :- L in 1..9, map_different(L).
map_diff([]) :- !.
map_diff([H|T]) :- diff(H), map_diff(T).

% Rows
first(L, N, R, RS) :- length(R, N), append(R, RS, L).
chop(L, [R|RS]) :- first(L, 9, R, Rest), !, chop(Rest, RS).
chop(_, []).

rows( L) :- chop(L, Rows), map_diff(Rows).

% Cols
join([], [], []) :- !.
join([H|T], [F|R], [[H|F]|B]) :- join(T, R, B).

emptys(0, []) :- !.
emptys(N, [[]|T]) :- N > 0, N1 is N-1, emptys(N1, T).

alternate([], N, Cols) :- !, emptys(N, Cols).
alternate(L, N, Cols) :-
	first(L, N, F, Rest),
	alternate(Rest, N, R),
	join(F, R, Cols).

cols(L) :- alternate(L, 9, Cols), map_diff(Cols).

% Boxes
boxes([], [], [], []) :- !.
boxes(R1, R2, R3, [B|Boxes]) :-
	first(R1, 3, S1, T1),
	first(R2, 3, S2, T2),
	first(R3, 3, S3, T3),
	append(S2, S3, S23), append(S1, S23, B),
	boxes(T1, T2, T3, Boxes).

collect([], []) :- !.
collect(L, Boxes) :- 
	first(L, 9, R1, L1),
	first(L1, 9, R2, L2),
	first(L2, 9, R3, L3),
	boxes(R1, R2, R3, B),
	collect(L3, Box),
	append(B, Box, Boxes).

box(L) :- collect(L, Boxes), map_diff(Boxes).

% Ugh... CBA
sudoku(L) :- box(L), rows(L), cols(L).

% Send more money
solve1([S,E,N,D],[M,O,R,E],[M,O,N,E,Y], Base) :-
	V1  = [E,N,D,O,R,Y],
	V2  = [S, M],
	append(V1, V2, Var),
	Base > 1, Up is Base-1,
	V1 in 0..Up, V2 in 1..Up, map_different(Var),
	B2 is Base * Base, B3 is Base * B2, B4 is B2 * B2,
		B3*S + B2*E + Base*N + D +
		B3*M + B2*O + Base*R + E #=
	B4*M + B3*O + B2*N + Base*E + Y,
	labeling([],Var).
