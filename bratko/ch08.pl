% 8.1 sub1 and sub3 behave nearly identically, sub2, I think is less efficient
% because it needs to visit the end of/empty list more often?
% The trace is nearly identical (bagof, number of lines) so I don't know,
% sub1 (106L) shows no failures but sub2 (130L) and (129L) are similar.
conc([], L, L).
conc([H|T], L, [H|R]) :- conc(T, L, R).

% prefix for sub1
prefix(_, []).
prefix([X|T], [X|S]) :- prefix(T, S).

% sub1
sub1(L, S) :- prefix(L, S).
sub1([_|L], S) :- sub1(L, S).

% sub2
sub2(L, S) :- conc(L1, _, L), conc(_, S, L1).

% sub3
sub3(L, S) :- conc(_, L2, L), conc(S, _, L2).

% 8.2) Remember to define them as [1,2,3|T]-T, so T is a *list*. Assuming
% strict left to right evaluation, the following given solution makes sense:
add_at_end(A-[Item|Z], Item, A-Z).

% 8.3) Once difference lists are understood, second clause is easy.
reverse(A-Z, L-L) :- A == Z, !. % A and Z are identical
reverse([H|T]-L, R-Z) :- reverse(T-L, R-[H|Z]).

% 8.4)
% makelist(L) :- collect([germany|T]-T, L-[]),
%
% collect(A-Z, Closed-C) :- A == Z, !.
%
% collect([X|T]-Open, Closed-C) :-
%	member(X, Closed-C), !,
%	collect(T-Open, Closed-C).
%
% collect([X|T]-Open, Closed-C) :-
%	ngb(X, Ngbs-T), !,
%	collect(Ngbs-Open, [X|Closed]-C).

% 8.5)
max([], Max, Max) :- !.
max([X|T], N, Max) :- X >= N, !, max(T, X, Max).
max([X|T], N, Max) :- N >= X, !, max(T, N, Max).

max([X|T], Max) :- max(T, X, Max).

% 8.6) Reworking 8-Queens. Reading the section properly *before* looking at
% what is in retrospect, an obvious solution, would have been wise.

% Writing (arg) to uninitialised variables is a success, otherwise it fails.
% Simple swap delete, but the numbers (1 <= X,Y <= 8) so:
%			X-Y -> X-Y+9 (-> X+Y+1)
%			X+Y -> X+Y-1.

% Ylist is a list of Y-coordinates of eight non-attacking queens
solution( Ylist)  :-   
  functor(Du, u, 15),		% Upward diagonals
  functor(Dv, v, 15),		% Downward diagonals 
  sol( Ylist,               % Y-coordinates of queens
       [1,2,3,4,5,6,7,8],   % Domain for X-coordinates
       [1,2,3,4,5,6,7,8],   % Domain for Y-coordinates
	   Du,
	   Dv).

sol( [], [], Dy, Du, Dv).

sol( [Y | Ylist], [X | Dx1], Dy, Du, Dv)  :-
  del( Y, Dy, Dy1),                 % Choose a Y-coordinate
  U is X-Y+9,                       % Corresponding upward diagonal
  arg(U, Du, 1),					% Remove it
  V is X+Y-1,                       % Corresponding downward diagonal
  arg(V, Dv, 1),					% Remove it
  sol( Ylist, Dx1, Dy1, Du, Dv).	% Use remaining values 

del( Item, [Item | List], List).

del( Item, [First | List], [First | List1] )  :-
   del( Item, List, List1).

% timing for original:
% 3 ?- time(bagof(S, solution(S), L)).
% 120,543 inferences, 0.031 CPU in 0.037 seconds (84% CPU, 3857376 Lips)

% time for new:
% 60 ?- time(bagof(S, solution(S), L)).
% 747,058 inferences, 0.297 CPU in 0.313 seconds (95% CPU, 2516406 Lips)

% 8.7) Don't waste your time with difference lists.
%	   It's just not possible, or elegant in this case.
add(Val, [_|T]) :- nonvar(T), !, add(Val, T).
add(Val, [_|T]) :- var(T), T=[Val|Z].

arr_upd(Func, Ind, Val) :- arg(Ind, Func, [Val|T]), !.
arr_upd(Func, Ind, Val) :- arg(Ind, Func, A), add(Val, A).
