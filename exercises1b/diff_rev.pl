% Reverse a difference list
reverse([], []).
reverse([H|T], L) :- rev(T, Trev), append(Trev, H, L).

concat(A-B, B-C, A-C).

empty_diff(A-A1) :- unify_with_occurs_check(A,A1).
diff_rev(L, T) :- empty_diff(L), empty_diff(T), !.
diff_rev([H|T]-T1, L) :-
	diff_rev(T-T1, Trev-T1rev),
	concat(Trev-T1rev, [H|T2]-T2, L).
