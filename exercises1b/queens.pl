:- [perm].

% check not equal over list
checkAttack(_, _, []) :- !.
checkAttack(Q, N, [H|T]) :- abs(Q-H, A), A =\= N, N1 is N+1, checkAttack(Q, N1, T).

checkDiag([]) :- !.
checkDiag([H|T]) :- checkAttack(H, 1, T), checkDiag(T).

enumerate(0, []) :- !.
enumerate(N, [N|T]) :- N > 0, N1 is N-1, enumerate(N1, T).

n_queens(N, R) :- enumerate(N, L), perm(L, R), checkDiag(R).
