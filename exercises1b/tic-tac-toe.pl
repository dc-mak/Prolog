% Tic-tac-toe. X goes first.
startBoard([e,e,e,e,e,e,e,e,e]).

e.
p(x).
p(o).

% columns
someWin([p(P), _, _,	p(P), _, _,		p(P), _, _]).
someWin([_, p(P), _,	_, p(P), _,		_, p(P), _]).
someWin([_, _, p(P),	_, _, p(P),		_, _, p(P)]).
% rows
someWin([p(P), p(P), p(P),	_, _, _,		_, _, _]).
someWin([_, _, _,	p(P), p(P), p(P),		_, _, _]).
someWin([_, _, _,		_, _, _,	p(P), p(P), p(P)]).
% diagonals
someWin([p(P), _, _,	_, p(P), _,		_, _, p(P)]).
someWin([_, _, p(P),	_, p(P), _,		p(P), _, _]).

% whose turn?
xTurn([], _, _) :- !.
xTurn([p(x)|T], Xs, Os) :- !, X1 is Xs + 1, xTurn(T, X1, Os).
xTurn([p(o)|T], Xs, Os) :- !, O1 is Os + 1, xTurn(T, Xs, Os).
xTurn([e|T], Xs, Os) :- xTurn(T, Xs, Os).

% X's turn or O's turn
xTurn(L) :- xTurn(L, N, N).
oTurn(L) :- xTurn(L, N, M), N is M+1.

insert([e|T], p(P), [p(P)|T]).
insert([H|T], p(P), [H|R]) :- insert(T, p(P), R).

% Naive
nextMove(Board, Move) :- xTurn(B), !, insertMove(Board, p(x), Move).
nextMove(Board, Move) :- oTurn(B), !, insertMove(Board, p(o), Move).

% for the non-naive version: make move, check, not("someWin for p(P)"),
% make all other moves, try for win
