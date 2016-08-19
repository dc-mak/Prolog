a(egg).
a(lentils).
a(cheese).
a(almonds).

collect(A, [H|T]) :- a(H), not(member(H,A)), !, collect([H|A], T).
collect(_, []).

everything(A) :- collect([], A).
