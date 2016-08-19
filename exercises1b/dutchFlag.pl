:- [perm].
% check colours
checkColors(L) :- checkRed(L).

checkRed([red|T]) :- checkRed(T).
checkRed(L) :- checkWhite(L).

checkWhite([white|T]) :- checkWhite(T).
checkWhite(L) :- checkBlue(L).

checkBlue([]).
checkBlue([blue|T]) :- checkBlue(T).
