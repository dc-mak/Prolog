% Reference/utilities.
app(A-B, B-C, A-C).

empty(A-A1) :- unify_with_occurs_check(A,A1).

diff([], A-A).
diff([H|T], [H|L]-A) :- diff(T, L-A).

% Minimum
list_min_([H], H).
list_min_([H|T], M) :- list_min_(T, N), M is min(H,N).

list_min([H|T]-T, H)  :- empty(T), !.
list_min([H|T]-T1, M) :- list_min(T-T1, N), M is min(H,N).

% Quicksort: Write it with a normal list.
part_([], [], _, []) :- !.
part_([H|T], [H|L], P, R) :- H =< P, !, part_(T, L, P, R).
part_([H|T], L, P, [H|R]) :- P <  H, !, part_(T, L, P, R).

qsort_([], []) :- !.
qsort_([P|T], Ans) :- 
	part_(T, L, P, R),
	qsort_(L, L1),
	qsort_(R, R1),
	append(L1, [P|R1], Ans).

% Quicksort: Now with difference lists.
% part(T, L, _, R) :- empty(T), empty(L), empty(R), !. -- Non-grounding
part([]-[], L-L, _, R-R) :- !.
part([H|T]-T1, [H|L]-L1, P, R-R1) :- H =< P, !, part(T-T1, L-L1, P, R-R1).
part([H|T]-T1, L-L1, P, [H|R]-R1) :- P <  H, !, part(T-T1, L-L1, P, R-R1).

/*
qsort(E, E) :- empty(E), !.
qsort([P|T]-T1, Ans-A1) :- 
	part(T-T1, L-L1, P, R-R1),
	qsort(L-L1, F-F1),
	qsort(R-R1, B-B1),
	app(F-F1, [P|B]-B1, Ans-A1).
*/

% qsort(E, E) :- empty(E), !. -- Non-grounding
qsort([]-[], E-E) :- !.
qsort([P|T]-T1, F-B1) :- 
	part(T-T1, L-L1, P, R-R1),
	qsort(L-L1, F-[P|B]),
	qsort(R-R1, B-B1).

% Mergesort: with normal lists.
split_([], [], []) :- !.
split_([H], [H], []) :- !.
split_([H,I|T], [H|L], [I|R]) :- split_(T, L,R).

merge_(L, [], L) :- !.
merge_([], R, R) :- !.
merge_([L|Tl], [R|Tr], [L|Merged]) :- L =< R, !, merge_(Tl, [R|Tr], Merged).
merge_([L|Tl], [R|Tr], [R|Merged]) :- L >  R, !, merge_([L|Tl], Tr, Merged).

msort_([], []) :- !.
msort_([H], [H]) :- !.
msort_(In, Sorted) :-
	split_(In, L, R),
	msort_(L, F),
	msort_(R, B),
	merge_(F, B, Sorted).

% Mergesort: with difference lists
% split(E, L, R) :- empty(E), empty(L), empty(R), !. -- Non-grounding
split([]-[], L-L, R-R) :- !.
split([H]-[], [H|T]-T, R) :- empty(T), empty(R), !.
split([H,I|T]-T1, [H|L]-L1, [I|R]-R1) :- split(T-T1, L-L1, R-R1).

merge(L-L1, E, L-L1) :- empty(E), !.
merge(E, R-R1, R-R1) :- empty(E), !.
merge([L|Tl]-Tl1, [R|Tr]-Tr1, [L|M]-M1) :-
	L =< R, !, merge(Tl-Tl1, [R|Tr]-Tr1, M-M1).
merge([L|Tl]-Tl1, [R|Tr]-Tr1, [R|M]-M1) :-
	L >  R, !, merge([L|Tl]-Tl1, Tr-Tr1, M-M1).

% msort(E, E) :- empty(E), !. -- Non-grounding
% msort([H|T]-T, [H|T]-T) :- empty(T), !. -- Non-grounding
msort([]-[], E-E) :- !.
msort([H]-[], [H|T]-T) :- !.
msort(I-I1, S-S1) :-
	split(I-I1, L-L1, R-R1),
	msort(L-L1, F-F1),
	msort(R-R1, B-B1),
	merge(F-F1, B-B1, S-S1).

% Andy's challenge
rev_([], []) :- !.
rev_([H|T], R) :- rev(T, Trev), append(Trev, [H], R).

rev([]-[], E-E) :- !.
rev([H|T]-T1, Tr-A) :- rev(T-T1, Tr-[H|A]).

% Hanoi
hanoi_(([H|L], M, R), 1, (L, M, [H|R])) :- !.
hanoi_((L, M, R), N, (L2, M2, R2)) :- 
	N > 1, N1 is N-1,
	hanoi_((L, R, M), N1, ([H|L1], R1, M1)),
	hanoi_((M1, L1, [H|R1]), N1, (M2, L2, R2)).

hanoi(([H|L]-T, M, R-S), 1, (L-T, M, [H|R]-S)) :- !.
hanoi((L, M, R), N, (L2, M2, R2)) :- 
	N > 1, N1 is N-1,
	hanoi((L, R, M), N1, ([H|L1]-T1, R1-S1, M1)),
	hanoi((M1, L1-T1, [H|R1]-S1), N1, (M2, L2, R2)).

% Dutch flag
:- [perm].
gather([], _, [], E-E) :- !.
gather([H|T], H, F, [H|R]-S) :- !, gather(T, H, F, R-S).
gather([P|T], H, [P|F], R-S) :- gather(T, H, F, R-S).

flag(In, Red) :- 
	gather(In, red, In1, Red-White),
	gather(In1, white, In2, White-Blue),
	gather(In2, blue, [], Blue-[]).
