prop(neg( and( neg(lit(p)), or( lit(q), neg( and( lit(r), lit(s) ) ) ) ) )).

nnfpos(lit(A), lit(A)).
nnfpos(neg(A), A1) :- nnfneg(A, A1).
nnfpos(and(A,B), and(A1, B1)) :- nnfpos(A,A1), nnfpos(B,B1).
nnfpos(or(A,B), or(A1,B1)) :- nnfpos(A,A1), nnfpos(B,B1).

nnfneg(lit(A), neg(lit(A))).
nnfneg(neg(A), A1) :- nnfpos(A,A1).
nnfneg(and(A,B), or(A1,B1)) :- nnfneg(A,A1), nnfneg(B,B1).
nnfneg(or(A,B), and(A1,B1)) :- nnfneg(A,A1), nnfneg(B,B1).

% so while this would backtrack, since it's used only for one application in
% apply, this is perfectly acceptable
distrib(or(and(A,B), and(C,D)), and(and(or(A,C), or(A,D)), and(or(B,C), or(B,D)))).
distrib(or(A, and(B,C)), and(or(A,B), or(A,C))).
distrib(or(and(A,B), C), and(or(A,C), or(B,C))).

% so it doesn't backtrack to last apply(A,A).
apply(and(A,B), and(A1,B1)) :- apply(A,A1), apply(B,B1), !.
% if possible, distrib otherwise unify.
apply(or(A,B), X1) :- distrib(or(A,B),X), !, apply(X,X1).
apply(A,A).

cnf(P, P1) :- nnfpos(P,P0), apply(P0,P1).
