%  Figure 14.9  A CLP(FD) program for eight queens.


% 8 queens in CLP(FD)

solution( Ys, N)  :-		  % Ys is list of Y-coordinates of queens
  nlist( Ys, N),			  % There are N queens
  Ys ins 1..N,				  % All the coordinates have domains 1..8
  all_different( Ys),         % All different to avoid horizontal attacks
  safe( Ys),                  % Constrain to prevent diagonal attacks
  mid_out(1, N, Ys).			 % Find concrete values for Ys

nlist([], 0) :- !.
nlist([_|T], N) :- N1 is N-1, nlist(T, N1).

interleave([], L, L) :- !.
interleave(L, [], L) :- !.
interleave([H1|L], [H2|U], [H1,H2|R]) :- interleave(L, U, R).

mid_out(L, U, Mo) :-
	MidL is U // 2, MidU is MidL +1,
	numlist(L, MidL, Lower),
	numlist(MidU, U, Upper),
	reverse(UpperRev, Upper),
	reverse(LowerRev, Lower),
	interleave(LowerRev, UpperRev, Mo).


safe( []).
safe( [Y | Ys])  :-
  no_attack( Y, Ys, 1),       % 1 = horizontal distance between queen Y and Ys
  safe( Ys).

% no_attack( Y, Ys, D):
%   queen at Y doesn't attack any queen at Ys; 
%   D is column distance between first queen and other queens

no_attack( Y, [], _).

no_attack( Y1, [Y2 | Ys], D)  :-
  D #\= Y1-Y2,
  D #\= Y2-Y1,
  D1 is D+1,
  no_attack( Y1, Ys, D1).
