% Graphs supervision work

% Missionaries

% Start and End states
startMiss((3, 3, l)).
endMiss((0, 0, _)).

% Valid states
oppSide(l,r) :- !.
oppSide(r,l).
opp((M,C,P), (M_, C_, P_)) :- M_ is 3-M, C_ is 3-C, oppSide(P, P_).

inRange(A) :- A >= 0, A =< 3.
safeState(M,C) :- M >= C.
safeState(0,_).
safeMove(M,C) :-
	inRange(M), inRange(C), safeState(M,C),
	opp((M,C,_), (M_, C_, _)), safeState(M_, C_).

% Valid rules/transitions
boatTrip(A) :- member(A, [(2,0), (1,0), (1,1), (0,1), (0,2)]).
moveMiss((M,C,l), (M1,C1,r)) :-
	boatTrip((A,B)), M1 is M-A, C1 is C-B, safeMove(M1, C1).
moveMiss((M,C,r), (M1,C1,l)) :-
	boatTrip((A,B)), M1 is M+A, C1 is C+B, safeMove(M1, C1).

% Keeping track of visited nodes
solveRecMiss(Prev, _, [End]) :-
	endMiss(End),
	moveMiss(Prev, End).
solveRecMiss(Prev, Visited, [Next|Path]) :-
	moveMiss(Prev, Next),
	not(member(Next, Visited)),
	solveRecMiss(Next, [Next|Visited], Path).

solveMiss([Start|Path]) :-
	startMiss(Start),
	solveRecMiss(Start, [Start], Path),
	showMiss([Start|Path]).

% Pretty printing
showP(0, _) :- write("---"), !.
showP(1, S) :- write(S), write("--"), !.
showP(2, S) :- write(S), write(S), write("-"), !.
showP(3, S) :- write(S), write(S), write(S).

showSide((M,C,l)) :- showP(M,"M"), write(" "), showP(C,"C"), !.
showSide((M,C,r)) :-
	showP(M,"M"), write(" "),
	showP(C,"C").

showMiss([H,I|T]) :-
	showSide(H), write(" |::<--::| "), opp(H,H2), showSide(H2), nl,
	showSide(I), write(" |::-->::| "), opp(I,I2), showSide(I2), nl,
	showMiss(T).

showMiss([]).

% Towers of Hanoi. The ring i is at position i in the list.
% The value at position i in the list is the tower

% Start and End states
startHan(([1,2,3], [], [])).
endHan(([], [], [1,2,3])).

% Transitions
okHan([_]) :- !.
okHan([H,A|_]) :- H < A.

moveHan(([H|T], M, B), (T, M, [H|B])) :- okHan([H|B]).
moveHan(([H|T], M, B), (T, [H|M], B)) :- okHan([H|M]).
moveHan((T, [H|M], B), (T, M, [H|B])) :- okHan([H|B]).
moveHan((T, M, [H|B]), ([H|T], M, B)) :- okHan([H|T]).
moveHan((T, [H|M], B), ([H|T], M, B)) :- okHan([H|T]).
moveHan((T, M, [H|B]), (T, [H|M], B)) :- okHan([H|M]).

solveRecHan(Prev, _, [End]) :- endHan(End), moveHan(Prev, End).
solveRecHan(Prev, Visited, [Next|Path]) :-
	moveHan(Prev, Next),
	not(member(Next, Visited)),
	solveRecHan(Next, [Next|Visited], Path).

showHan([]).
showHan([H|T]) :- write(H), nl, showHan(T).

solveHan([S|L]) :- startHan(S), solveRecHan(S, [S], L), showHan([S|L]).

% Generalised Hanoi

% Start and End
startPole(0,L,L) :- !.
startPole(N,L,R) :- N1 is N-1, startPole(N1, [N|L], R).

emptys(0, L, L) :- !.
emptys(N, L, R) :- N1 is N-1, emptys(N1, [[]|L], R).

startGenHan(N, [H|T]) :- N >= 3, startPole(N,[],H), N1 is N-1, emptys(N1,[],T).
endGenHan(N, L) :- startGenHan(N, S), reverse(S, L).

% Rules
split(T, [], T).
split([H|T], [H|R], S) :- split(T, R, S).

tryHan(Pre, H, A, Post, S) :-
	split(Pre, Bef, [Pole|After]), 
	okHan([H|Pole]),
	append(Bef, [[H|Pole]|After], Pre2),
	append(Pre2, [A|Post], S).
tryHan(Pre, H, A, Post, S) :-
	split(Post, Bef, [Pole|After]), 
	okHan([H|Pole]),
	append(Bef, [[H|Pole]|After], Post2),
	append(Pre, [A|Post2], S).

moveGenHan(L, S) :-
	split(L, Pre, [[H|A]|Post]),
	tryHan(Pre, H, A, Post, S).

solveRecGenHan(N, Prev, _, [End]) :- endGenHan(N, End), moveGenHan(Prev, End).
solveRecGenHan(N, Prev, Visited, [Next|Path]) :-
	moveGenHan(Prev, Next),
	not(member(Next, Visited)),
	solveRecGenHan(N, Next, [Next|Visited], Path).

solveGenHan(N, [S|L]) :- startGenHan(N,S), solveRecGenHan(N, S, [S], L), showHan([S|L]).

% Umbrella
% Auxilliary clauses
take([H|T], H, T).
take([H|T], R, [H|S]) :- take(T, R, S).

choose(0, L, [], L) :- !.
choose(N, [H|T], [H|Chosen], Rest) :-
	N > 0, N1 is N-1, choose(N1, T, Chosen, Rest).
choose(N, [H|T], Chosen, [H|Rest]) :-
	N > 0, choose(N, T, Chosen, Rest).

% perm([], []) :- !.
% perm(T, [S|R]) :- take(T, S, L), perm(L,R).

% Folk at the house
restUmbRec([], _, []).
restUmbRec([R|Rem], A, L) :- member(R, A), !, restUmbRec(Rem, A, L).
restUmbRec([R|Rem], A, [R|L]) :- restUmbRec(Rem, A, L).

restUmb(A, L) :- startUmb((Full,_)), restUmbRec(Full, A, L).

% Start and end states
startUmb(([(a,1),(b,2),(c,5),(d,10)],car)).
endUmb(([],_)).

% Transitions
moveUmb((Bef,car), (Aft,hos)-Time) :-
	choose(2, Bef, [(A,T), (B,P)], Aft), 
	Time is max(T,P).
moveUmb((Bef,hos), ([(P,Time)|Bef],car)-Time) :-
	restUmb(Bef,AtH),
	take(AtH, (P,Time), _).

% To avoid the semantics of perm and then not(member).
notVisited(Next, []).
notVisited(Next, [N|T]) :- permutation(Next, N), !, fail.
notVisited(Next, [_|T]) :- notVisited(Next, T).

% Add up times
totalList(L, N) :- totalList(L, 0, N).
totalList([], R, R).
totalList([_-N|T], SoFar, R) :- S2 is N+SoFar, totalList(T, S2, R).

% Show solution fully
showList([]).
showList([H|T]) :- write(H), nl, showList(T).

% Solve
solveRecUmb(Prev, _, [End-T]) :- endUmb(End), moveUmb(Prev, End-T).
solveRecUmb(Prev, Vis, [(Next,P)-T|Rest]) :-
	moveUmb(Prev, (Next,P)-T),
	notVisited(Next, Vis),
	solveRecUmb((Next,P), [Next|Vis], Rest).

solveUmb([S-0|L], N) :-
	startUmb(S), solveRecUmb(S, [S], L), totalList(L,N), showList([S-0|L]).
