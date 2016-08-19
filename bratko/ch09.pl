% 9.1)
merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([H|T], [A|B], [H|R]) :- H =< A, !,  merge(T, [A|B], R).
merge([H|T], [A|B], [A|R]) :- A =< H, !,  merge([H|T], B, R).

% 9.2)
concat(A-X, X-Z, A-Z).

gt(X,Y) :- number(X),number(Y), X >= Y.

qsort(A-Z, L-L) :- A == Z, !.
qsort([X|Tail]-Rest, Sorted-R) :-
	split(X, Tail-Rest, Small-S, Big-B),
	qsort(Small-S, SortSmall-SS),
	qsort(Big-B, SortBig-SB),
	concat(SortSmall-SS, [X|SortBig]-SB, Sorted-R).

split(_, A-Z, L-L, M-M) :- A==Z, !.
split(X, [Y|Tail]-R, [Y|Small]-S, Big-B) :-
	gt(X,Y), !,
	split(X, Tail-R, Small-S, Big-B).
split(X, [Y|Tail]-R, Small-S, [Y|Big]-B) :-
	split(X, Tail-R, Small-S, Big-B).

% 9.3) Uneven 'split's, an element at a time so n^2.

% 9.4)
mergesort([], []) :- !.
mergesort([X], [X]) :- !.	% Important clause I forgot - to avoid cycle
mergesort(L, Sort) :-
	half(L, Left, Right),
	mergesort(Left, LeftSrt),
	mergesort(Right, RightSrt),
	merge(LeftSrt, RightSrt, Sort).

half([], [], []) :- !.
half([X], [X], []) :- !.
half([X,Y|T], [X|Left], [Y|Right]) :- half(T, Left, Right).

% 9.5) a)
binarytree(nil).
binarytree(t(L, _, R)) :- binarytree(L), binarytree(R).

% 9.5) b) 0, 00, 01, 10, 11.
dictionary(nil) :- !.
dictionary(t(nil, X, nil)) :- !.
dictionary(t(nil, X, t(Rl, R, Rr))) :-
	X < R, dictionary(Rl), dictionary(Rr).
dictionary(t(t(Ll, L, Lr), X, nil)) :-
	L =< X, dictionary(Ll), dictionary(Lr).
dictionary(t(t(Ll, L, Lr), X, t(Rl, R, Rr))) :-
	L =< X, X < R,
	dictionary(Ll), dictionary(Lr),
	dictionary(Rl), dictionary(Rr).

% 9.6)
height(nil, 0).
height(t(L, _, R), N) :- height(L, HL), height(R, HR), N is max(HL, HR)+1.

% 9.7) Inefficient
n_preorder(nil, []).
n_preorder(t(L, X, R), List) :-
	n_preorder(L, Left), n_preorder(R, Right), append([X|Left], Right, List).

% Efficient (and more elegant)
preord(nil, L, L) :- !.
preord(t(L, X, R), Sofar, [X|Pre]) :-
	preord(R, Sofar, Right), preord(L, Right, Pre).
preorder(X, L) :- preorder(X, [], L).

n_inorder(nil, []).
n_inorder(t(L, X, R), List) :-
	n_inorder(L, Left), n_inorder(R, Right), append(Left, [X|Right], List).

inord(nil, Sofar, L) :- !, Sofar = L.
inord(t(L, X, R), Sofar, In) :- inord(R, Sofar, Right), inord(L, [X|Right], In).
inorder(X, L) :- inord(X, [], L).

n_postorder(nil, []).
n_postorder(t(L, X, R), List) :-
	n_postorder(L, Left), n_postorder(R, Rite),
	append(Rite, [X], Right), append(Left, Right, List).

% Efficient (and more elegant)
postord(nil, L, L) :- !.
postord(t(L, X, R), Sofar, Post) :-
	postord(R, [X|Sofar], Right), postord(L, Right, Post).
postorder(X, L) :- postord(X, [], L).

% 9.9)
in(X, t(_, X, _), Path, [X|Path]) :- !.
in(X, t(L, Y, _), Path, Result) :- Y =< X, !, in(X, L, [X|Path], Result).
in(X, t(L, Y, _), Path, Result) :- Y > X, !, in(X, R, [X|Path], Result).
in(X, Dict, Path) :- in(X, Dict, [], Path).

% 9.10) Here goes.
% p : atom * length * level/depth

tag(nil, _, nil) :- !.
tag(t(L, X, R), Level, t(Li, p(X, N, Level), Ri)) :-
	atom_length(X, N), L2 is Level + 1, tag(L, L2, Li), tag(R, L2, Ri).

tagged_list(X, X_List) :- tag(X, 0, X_Tagged), inorder(X_Tagged, X_List).

max_depth_rec([], Max, Max) :- !.
max_depth_rec([p(_,_,N)|T], Sofar, Max) :- N > Sofar, !, max_depth_rec(T, N, Max).
max_depth_rec([p(_,_,N)|T], Sofar, Max) :- Sofar >= N,   max_depth_rec(T, Sofar, Max).

max_depth(List, Max) :- max_depth_rec(List, 0, Max).

write_blank(0) :- !.
write_blank(N) :- N > 0, write(' '), N1 is N-1, write_blank(N1).

write_row([], _) :- !, nl, nl.
% p : atom * length * level/depth
write_row([p(X,_,L)|T], D) :- L =:= D, !, write(X), write(' '), write_row(T, D).
write_row([p(X,N,L)|T], D) :- L =\= D, write_blank(N+1), write_row(T, D).

write_tree_rec(List, D, D) :- write_row(List, D), true.
write_tree_rec(List, Level, D) :-
	write_row(List, Level), L1 is Level+1, write_tree_rec(List, L1, D).

write_tree(Tree) :-
	tagged_list(Tree, List),
	max_depth(List, Depth),
	write_tree_rec(List, 0, Depth).

% For constructing trees
half_list([], Length, [], [], Half) :- !, Half is Length // 2.
half_list([H|T], Pos, [H|Left], Right, Half) :-
	P1 is Pos+1, half_list(T, P1, Left, Right, Half), Pos < Half, !.
half_list([H|T], Pos, Left, [H|Right], Half) :-
	P1 is Pos+1, half_list(T, P1, Left, Right, Half).

fromIn([], nil) :- !.
fromIn(List, t(Tr_L, X, Tr_R)) :-
	half_list(List, 0, Left, [X|Right], _),
	fromIn(Left, Tr_L),
	fromIn(Right, Tr_R).

:- T = [this, is, a, list, of, atoms, of, varying, 1, length, '.'],
	write(T), nl, nl, fromIn(T, Tr), write_tree(Tr).

% Given solution, for comparison
show(Tree) :- dolevels(Tree, 0, more).

dolevels(Tree, Level, alldone) :- !.
dolevels(Tree, Level, more) :-
	traverse(Tree, Level, 0, Continue), nl,
	NextLevel is Level + 1,
	dolevels(Tree, NextLevel, Continue).

traverse(nil, _, _, _).
traverse(t(Left, X, Right), Level, Xdepth, Continue) :-
	NextDepth is Xdepth+1,
	traverse(Left, Level, NextDepth, Continue),
	(Level = Xdepth, !,
		write(X), Continue = more
	;
		write(' ')),
	traverse(Right, Level, NextDepth, Continue).

:- T = [a,l,i,s,t,o,f,a,t,o,m,s,o,f,w,i,d,t,h,'1','!'],
	write(T), nl, nl, fromIn(T, Tr), show(Tr).

% 9.11) Min-cost.
% For both the algorithmic and the declarative way, the important bit is 
% modifiying addedge with the following constraints.

% addedge(Tree, NewTree, Graph):
%   add an edge from Graph to Tree without creating a cycle
%   ensure that such added edge has minimal cost (Kruskal ftw).

% Perfect place to use CLP, just minimise cost and be done.
% addedge(Tree, [A-B-Cost | Tree], Graph)  :-
% 	adjacent(A, B, Cost, Graph),    % Nodes A and B adjacent in Graph   
% 	node(A, Tree),                  % A in Tree   
% 	not(node(B, Tree)),             % A-B doesn't create a cycle in Tree 
% 	not(adjacent(A, Z, C2, Graph),  % There is no other edge that satisfies
% 		C2 < Cost).					% these constraints with lower cost

% 9.22) These results don't make sense. Clearly, there is something wrong
% with one of the procedures (I suspect 9_23) so that the same solution(s)
% are not being generated.
:- ['./programs/fig9_22'],
	Gr = [a-c, c-d, e-c, h-i, h-f, f-g, a-e, g-c],
 	time(bagof(Tr1, stree(Gr, Tr1), Trees1)), write(Trees1).
:- ['./programs/fig9_20','./programs/fig9_23'],
	Gr = [a-c, c-d, e-c, h-i, h-f, f-g, a-e, g-c, a-h],
	time(bagof(Tr1, stree(Gr, Tr1), Trees1)), write(Trees1).
