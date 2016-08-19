conc([], Concat, Concat).
conc([Head|Tail], Rest, [Head|Concat]) :- conc(Tail, Rest, Concat).

% This ideally would use a cut.
member(X,L) :- conc(_, [X|_], L).

% 3.1) a) Delete the last three elements from a list.
delete_last_three(L,L1) :- conc(L1,[_,_,_],L).

% 3.1) b) Delete first and last three from a list.
trim_three(L,L2) :- conc([_,_,_|L2],[_,_,_],L).

% 3.2) a) Last with conc
last_conc(L,X) :- conc(_, [X], L).

% 3.2) b) Last without conc
last(X, [X]).
last(X,[_|Tail]) :- last(X, Tail).

% 3.3) Initial attempt.
even_list([]).
even_list([_,_|L]) :- even_list(L).

odd_list([_|T]) :- even_list(T).

% After I found out mutual recursion was possible.
evenlength([]).
evenlength([_|Tail]) :- oddlength(Tail).

oddlength([_|T]) :- evenlength(T).

% 3.4) I don't like the stack-trace of this.
reverse([], []).
reverse([H|T],R) :- reverse(T,F),conc(F,[H],R).

% 3.5) Although I am happy with this.
palindrome(L) :- reverse(L,L).

% 3.6)
shift([]).
shift([Head|Tail], List) :- conc(Tail, [Head], List).

% 3.7)
means(0, zero).
means(1, one).
means(2, two).
means(3, three).
means(4, four).
means(5, five).
means(6, six).
means(7, seven).
means(8, eight).
means(9, nine).

translate([], []).
translate([Head|Tail], [Trans|List]) :- means(Head,Trans),translate(Tail,List).

% 3.8) Was on the wrong track, question is more focused on being able to generate
%	   subsets than proving one is a subset of another.
subset([],[]).
subset([First|Rest], [First|Sub]) :- subset(Rest,Sub).
subset([_|Rest], Sub) :- subset(Rest,Sub).

% 3.9)
dividelist([], [], []).
dividelist([H], [H], []).
dividelist([H,S|T], [H|L], [S|R]) :- dividelist(T,L,R).

% 3.10) monkey-banana with a list to keep track of moves.
move(state(middle, onbox, middle, hasnot),
	 grasp,
	 state(middle, onbox, middle, has)).

move(state(P, onfloor, P, H),
	 climb,
	 state(P, onbox, P, H)).

move(state(P1, onfloor, P1, H),
	 push(P1, P2),
	 state(P2, onfloor, P2, H)).

move(state(P1, onfloor, B, H),
	 walk(P1,P2),
	 state(P2, onfloor, B, H)).

canget(state(_, _, _, has), []).
canget(State1,[Move|L]) :- move(State1, Move, State2),canget(State2,L).

canget_nolist(state(_,_,_,has)).
canget_nolist(State1) :- move(State1, _, State2),canget_nolist(State2).

% 3.11) Flatten an aribitrarily nested list.
flatten([Head|Tail], FlatList) :-
	flatten(Head,FlatHead),
	flatten(Tail,FlatTail),
	conc(FlatHead,FlatTail,FlatList).
flatten([],[]).
% Although I knew that non-lists were causing it to fail, here's
% the trick I missed, which makes complete sense in retrospect.
flatten(X,[X]).

% 3.12) Like so:
%	plays(jimmy, and(football, squash))
%	plays(susan, and(tennis, and(basketball, volleyball)))

% 3.13)	:- op (500, yfx, of).
%		:- op (600, fx, the).
%		:- op (700, xfx, was).

% 3.14)	a)	A = 1+0
%		b)	X = 0
%			X1 = 1+0
%			X' = 1
%			Z = 1+1+0
%		c)	t(1+0+1+1+1, C)	u t(X+1+1, Z) :- t(X+1, Y), t(Y+1, Z)
%				X=1+0+1, C = Z
%				t(1+0+1+1, Y) u t(X1+1+1, Z1) :- t(X1+1, Y1), t(Y1+1, Z1)
%					X1 = 1+0, Y=Z1
%					t(1+0+1, Y1) u t(X2+0+1, X2+1+0)
%						X2 = 1, Y1 = 1+1+0
%					t(1+1+0+1, Z1) u t(X3+0+1, X3+1+0)
%						X3=1+1, Z1 = 1+1+1+0
%				t(1+1+1+0+1, Z) u t(X4+0+1, X4+1+0)
%					X4=1+1+1, Z=1+1+1+1+0
%			C = 1+1+1+1+0
%		d)	Without going through all the working, I think it would do this:
%			1+1+1+0, 1+1+0+1, 1+0+1+1, 0+1+1+1

% 3.15)	terms: in, concatenating, and, gives, deleting, from
%		precedence: in, and, from, gives, {deleting, concatenating}
%		answer gave 500 and 600 operators same precedence 
:- op(500, xfx, in).
:- op(600, xfx, and).
:- op(600, xfx, from).
:- op(650, xfx, gives).
:- op(700, fx, deleting).
:- op(700, fx, concatenating).

X in [_|T] :- X in T.
X in [X|_].

concatenating [] and L gives L.
concatenating [H|T] and L gives [H|R] :- concatenating T and L gives R.

deleting X from [X|T] gives T.
deleting X from [_|T] gives R :- deleting X from T gives R.

% 3.16 Ideally this would use a cut.
max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- Y > X.

% 3.17 Again, this needs a cut. No real difference in efficiency.
maxlist([M],M). 
maxlist([H|T],M) :- maxlist(T,X),max(H,X,M).

maxlist2([M],M).
maxlist2([H,X|T], M) :- max(H,X,M),maxlist2([M|T], M).

maxlist_aux([],M,M).
maxlist_aux([H|T],N,M) :- max(H,N,Z),maxlist_aux(T,Z,M).

maxlist3([H|T],M) :- maxlist_aux(T,H,M).

% 3.18 No real difference in efficiency.
sumlist([], 0).
sumlist([H|T],N) :- sumlist(T,M), N is H+M.

sumlist2([],Res,Res).
sumlist2([H|T],Acc,N) :- M is Acc+H,sumlist2(T,M,N).

% 3.19 If max had cuts...
ordered([]).
ordered([_]).
ordered([H,X|T]) :- max(X,H,X),ordered(T).

% 3.20
subsum(Set,Sum,Subset) :- subset(Set,Subset),sumlist(Subset,Sum).

% 3.21: I kept trying to increment N, when it's the lower bound that needs
% to increase towards the limit - N is L+1, between(L,U,N). Note the strict
% inequality in the second clause, which is necessary for L+1 =< U.
between(L,U,L) :- L =< U.
between(L,U,N) :- L < U, L1 is L+1, between(L1,U,N).

% 3.22 Comparison operators are 700.
%	Explicit = so different variables can be unified and *then* equated.
:- op(700, xfx, :=).
:- op(800, xfx, else).
:- op(850, xfx, then).
:- op(900, fx, if).

if X > Y then Z := W else Z := _ :- X > Y, Z = W.
if X > Y then Z := _ else Z := W :- X =< Y, Z = W.
