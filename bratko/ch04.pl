% 4.1 a)
% no_children(SName) :- family(person(_,SName,_,_),_,[]).

% 4.1 b)
% child(person(_,_,_,works(_,_))).

% 4.1 c)
% family(person(_,SName,_,unemployed),person(_,_,_,work(_,_)),_).

% 4.1 d) Or write (Y1 - Y2 >= 15; Y2 - Y1 >= 15).
% diff_15(X,Y) :- X - Y >= 15.
% diff_15(X,Y) :- Y - X >= 15.

% family(person(_,_,date(_,_,Y1),_),
%		 person(_,_,date(_,_,Y2),_),
%		 Children),
% diff_15(Y1,Y2).

% 4.2) Got this wrong, need to ensure children are of same family and that
% each child is not counted twice.
% twins(C1,C2) :-
% 	family(_,_,Children),
% 	del(C1, Children, Rest),
% 	member(C2, Rest),
% 	dateofbirth(Child1,D),
% 	dateofbirth(Child2,D).

% 4.3)
nth_member(1,[X|_],X).
nth_member(N,[_|T],X) :- N > 1, M is N-1, nth_member(M,T,X).

% 4.4) The only cycle of arrows passed through the accepting state.

% 4.5) Add a condition MaxMoves > 0 for all clauses, and M2 is MaxMoves-1 for
% the recursive call.

% 4.6) It builds up the result in reverse, last item first, because of the
% recursive call to solution(Others) in solution. Could use conc function?
% NO: order determined by the member function: member(Y, [...]) so swap -
% member([...], Y).

% 4.7) a)
inrange(X) :- X >= 1, X =< 8.

moves(X,N,R_x) :- R_x is X-N.
moves(X,N,R_x) :- R_x is X+N.

jump(X1/Y1, X2/Y2) :-
	moves(X1,1,X2), moves(Y1,2,Y2),
	inrange(X2),inrange(Y2).

jump(X1/Y1, X2/Y2) :-
	moves(X1,2,X2),moves(Y1,1,Y2),
	inrange(X2),inrange(Y2).

% 4.7) b)
knightpath([H,I|T]) :- jump(H,I),knightpath([I|T]).
knightpath([_]).

S = [2/1,_,5/4,_,_/8], knightpath(S).
