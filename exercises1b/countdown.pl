% Countdown stuff
choose(0, L, [], L) :- !.
choose(N, [H|T], [H|R], S) :- N > 0, N1 is N-1, choose(N1, T, R, S).
choose(N, [H|T], R, [H|S]) :- choose(N, T, R, S).

diff(T, R, D) :- abs(T-R, D).

eval(plus(A,B), C) :- !, eval(A,Av), eval(B,Bv), C is Av+Bv.
eval(mult(A,B), C) :- !, eval(A,Av), eval(B,Bv), C is Av*Bv.
eval(sub(A,B), C) :- !,  eval(A,Av), eval(B,Bv), C is Av-Bv.
eval(div(A,B), C) :- !,  eval(A,Av), eval(B,Bv), C is Av//Bv.
eval(A, A).

arithop(A,B,plus(A,B)).
arithop(A,B,sub(A,B)) :- isGreater(A,B).
arithop(A,B,sub(B,A)) :- isGreater(B,A).
arithop(A,B,mult(A,B)) :- notOne(A), notOne(B).
arithop(A,B,div(A,B)) :- notOne(B), isFactor(B,A).
arithop(A,B,div(B,A)) :- notOne(A), isFactor(A,B).

isGreater(A,B) :- eval(A, Av), eval(B,Bv), Av > Bv.
notOne(A) :- eval(A, Av), Av =\= 1.
isFactor(A,B) :- eval(A, Av), eval(B, Bv), 0 is Bv rem Av.

solve2([Soln|_], Target, Soln, D) :- eval(Soln, R), diff(R, Target, D).
solve2(L, Target, Soln, D) :- choose(2, L, [A,B], R),
							  arithop(A, B, C),
							  solve2([C|R], Target, Soln, D).

closest(L, Target, Soln, D) :- range(0, 100, D), solve2(L, Target, Soln, D).

range(N, Lim, N) :- N < Lim.
range(N, Lim, D) :- N < Lim, N1 is N+1, range(N1, Lim, D).

countdown([Soln|_], Target, Soln) :- eval(Soln, Target).
countdown(L, Target, Soln) :- choose(2, L, [A,B], R),
							  arithop(A, B, C),
							  countdown([C|R], Target, Soln).
