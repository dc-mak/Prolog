% take - needed for perm
take([H|T], H, T).
take([H|T], R, [H|S]) :- take(T, R, S).

% same length
same_len([], []) :- !.
same_len([_|T], [_|B]) :- same_len(T, B).

% permutations
perm([], []) :- !.
perm(L, [H|T]) :- same_len(L, [H|T]), take(L, H, R), perm(R, T).
