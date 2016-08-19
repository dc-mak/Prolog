% For my own use/testing:
showR([]).
showR([1|T]) :- write('#'), !, showR(T).
showR([0|T]) :- write(' '),	  showR(T).

showR(L,R) :- showR([L,1,1,1,1,R]).

showP([_, [N, [_,R2,R3,R4,R5,_], S, [_,L5,L4,L3,L2,_]]]) :-
	showR(N), nl,
	showR([L2,1,1,1,1,R2]), nl,
	showR([L3,1,1,1,1,R3]), nl,
	showR([L4,1,1,1,1,R4]), nl,
	showR([L5,1,1,1,1,R5]), nl,
	reverse(S,RvS), showR(RvS), nl.

show2P([_, [N, [_,E2,E3,E4,E5,_], S, [_,W5,W4,W3,W2,_]]],
	   [_, [U, [_,R2,R3,R4,R5,_], D, [_,L5,L4,L3,L2,_]]]) :-
	reverse(S,RvS), reverse(D,RvD),

	showR(N),	  showR([0]), showR(U), nl,
	showR(W2,E2), showR([0]), showR(L2,R2), nl,
	showR(W3,E3), showR([0]), showR(L3,R3), nl,
	showR(W4,E4), showR([0]), showR(L4,R4), nl,
	showR(W5,E5), showR([0]), showR(L5,R5), nl,
	showR(RvS),	  showR([0]), showR(RvD), nl.

turns(N,O) :- piece([N,E]), orientation([N,E],O,OP), show2P([N,E],OP).

edges(P1, E1, P2, O, E2) :-
	piece([P1,Es1]), piece([P2,Es2]), P1 \= P2,
	orientation([P2,Es2], O, OP2),
	compatible([P1,Es1], E1, OP2, E2). 

test1 :-
	P0 = ['65',_], piece(P0), !,
	P1 = ['98',_], piece(P1), !,
	P2 = ['02',_], piece(P2), !,
	P3 = ['Cc',_], piece(P3), !,
	compatible(P0, 2, P1, 0),
	compatible_corner(P0, 3, P1, 0, P2, 1).

test :- findall(P, piece(P), Ps), puzzle(Ps, _).

puzzles([['74', [[1,1,0,0,1,0], [0,1,0,1,0,0], [0,1,0,0,1,0], [0,1,0,0,1,1]]],
		['65', [[1,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,1,0,1]]],
		['13', [[0,1,0,1,0,1], [1,1,0,1,0,1], [1,1,0,0,1,1], [1,1,0,0,1,0]]],
		['Cc', [[0,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,0,1,0], [0,0,1,1,0,0]]],
		['98', [[1,1,0,0,1,0], [0,1,0,0,1,0], [0,0,1,1,0,0], [0,0,1,1,0,1]]],
		['02', [[0,0,1,1,0,0], [0,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0]]]]).
test2 :-
	puzzles([P0, P1, P2, P3, P4, P5]),
	puzzle([P0, P1, P2, P3, P4, P5],
			[[P0, 0], [P1, 2], [P4, 3], [P2, 1], [P3, 0], [P5, 2]]).
