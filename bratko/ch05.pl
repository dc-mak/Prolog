% 5.1) a) 1,2.
%	   b) 1,1; 1,2; 2,1; 2,2
%	   c) 1,1; 1,2

% 5.2)	
class(Number, positive) :- Number > 0, !.
class(0, zero) :- !.
class(_, negative).

% 5.3) Red cuts all over the place.
split([], [], []) :- !.

split([H|T], [H|Pos], Neg) :-
	(class(H, positive) ; class(H,zero)),
	!,
	split(T, Pos, Neg).

split([H|T], Pos, [H|Neg]) :-
% class(H, negative), only necessary if cuts are not used.
	split(T, Pos, Neg).

% 5.4) Problem with not(member(H,S)) is it limits backtracking and so when it
% encounters an element that *is* in the list, it fails for good.
set_difference([], _, []).
set_difference([H|T], S, Diff) :- member(H, S), !, set_difference(T, S, Diff).
set_difference([H|T], S, [H|Diff]) :- set_difference(T, S, Diff).

% 5.5)
unifiable([],_,[]).
unifiable([H|T], X, Rest) :- not(X = H), !, unifiable(T,X,Rest).
unifiable([H|T], X, [H|Rest]) :- unifiable(T,X,Rest).
