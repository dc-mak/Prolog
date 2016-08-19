% 14.1) I think we get the same answer (pretty neat, what's the proof for it?)
% but I keep making mistakes -- solution: yes we do.

% 14.2)	Fixing Tf = 9, going anti-clockwise.
%		Tc = 2..4, Td = 5, Tb = 2, Ta = 0.

% 14.3) Potentially noting that N>1 and if there are more than 4 terms the
% criteria is unsatisfiable.

% 14.4) Modified scheduler.

% For normal:
% -----------
%	resource(_, [1,2,3]).
%	---------------------
%	278,167 inferences, 0.094 CPU in 0.102 seconds

% For the following:
% ------------------
%	resource(t7, [3]).
% 	resource(t6, [1,2]).
% 	resource(_, [1,2,3]).
%	---------------------
%	204,808 inferences, 0.078 CPU in 0.073 seconds
%	FinTime = 33
%	Schedule = [t1/2/_G191/4, t2/2/0/2, t3/1/0/2, t4/2/_G227/20,
%				t5/1/13/20, t6/1/2/11, t7/3/_G263/11]

:- use_module(library(clpq)).
:- ['./programs/fig14_4'].
% :- time(schedule(S,F)),
%	write('FinTime = '), write(F), nl,
%	write('Schedule = '), write(S).

% 14.5) Modified scheduler.
% Somewhat surprisingly, typical times are 50, 46, 44, 30, 26, 24.
% So until limit is reduced to 60, improvements aren't seen.

% For a upper limit of 25:
% ------------------------
% 39,725 inferences, 0.031 CPU in 0.024 seconds
% FinTime = 24
% Schedule = [t1/3/0/4, t2/2/0/2, t3/1/0/2, t4/3/4/20, t5/2/4/20,
%			  t6/1/2/11, t7/1/13/11]

% 14.6) Modified circuits.
:- ['./programs/fig14_6'].
:- ['./programs/fig14_7'].

%	circuit_a with two extra resistos in parallel:
%	=========
%	51 ?- circuit_a(2, 5, T).
%	T = (400 rdiv 71, 80 rdiv 71) ;

%	circuit_b
%	=========
%	With diode across T51-(wire: T5A-T5B)-(diode: ->):
%	--------------------------------------------------
%	53 ?- circuit_b(10, _, _, T31, T41, T51, T52).
%	T31 = (15 rdiv 2, 1 rdiv 2),
%	T41 = (5, 1 rdiv 2),
%	T51 = (15 rdiv 2, 0),
%	T52 = (5, 0) ;
%	
%	Without diode or T51-(wire: T5A-T5B)-(diode: <-):
%	-------------------------------------------------
%	55 ?- circuit_b(10, _, _, T31, T41, T51, T52).
%	T31 = (345 rdiv 47, 23 rdiv 47),
%	T41 = (245 rdiv 47, 49 rdiv 94),
%	T51 = (345 rdiv 47, 2 rdiv 47),
%	T52 = (245 rdiv 47, -2 rdiv 47) ;

% 14.7)
:- use_module(library(clpfd)).
:- ['./programs/fig14_8'].
% :- time(solve(N1,N2,N3)),
%		write('N1: '), write(N1), nl
%		write('N2: '), write(N2), nl
%		write('N3: '), write(N3).

% For [] in labeling:
% 15,118,363 inferences, 3.844 CPU in 3.837 seconds
% For [ff] in labeling:
% 93,243 inferences, 0.016 CPU in 0.024 seconds
% (so fast I thought it memoized results from previous run)

% 14.8)
:- ['./programs/fig14_9'].

% All the code I added :-)
nlist([], 0) :- !.
nlist([_|T], N) :- N1 is N-1, nlist(T, N1).

% middle out order: a "labeling order" is not very clear at all,
% it's simply an assigment with backtracking controlled in the way you want.
my_label(Ys, [Label]) :- !, labeling([Label], Ys).
my_label(Ys, middle_out) :-
	Ys = [H|_],
	fd_dom(H, L..U),
	mid_out(L, U, Mo),
	permutation(Ys, Mo).

% With this, specifying all_different is redundant.
mid_out(L, U, Mo) :-
	MidL is (U-1) // 2 + 1, MidU is MidL+1,
	rev_num_list(L, MidL, LowerRev),
	numlist(MidU, U, Upper),
	interleave(LowerRev, Upper, Mo).

% interleave upper and lower
interleave([], L, L) :- !.
interleave(L, [], L) :- !.
interleave([H1|L], [H2|U], [H1,H2|R]) :- interleave(L,U,R).

% nubmer list from high to low
rev_num_list(L, L, [L]) :- !.
rev_num_list(L, U, [U|R]) :- U1 is U-1, rev_num_list(L, U1, R).
% :- ['queens_works'].

% N generalisation with 12 queens, straight labeling:
:- write('Straight label: '),
   time(solution(Ys, 12, [ff])),
   write('Ys: '), write(Ys), nl.
% Straight label:
% 151,275 inferences, 0.031 CPU in 0.037 seconds
% Ys: [1,3,5,11,8,10,12,4,2,7,9,6]

% N generalisation with 12 queens, bisect labeling:
:- write('Bisect label: '),
   time(solution(Ys, 12, [bisect])),
   write('Ys: '), write(Ys), nl.
% Bisect label:
% 157,966 inferences, 0.047 CPU in 0.041 seconds
% Ys: [1,3,5,8,10,12,6,11,2,7,9,4]

% N generalisation with 12 queens, middle out labeling:
:- write('Middle-out label: '),
   time(solution(Ys, 12, middle_out)),
   write('Ys: '), write(Ys), nl.
% Middle-out label:
% 39,215 inferences, 0.000 CPU in 0.009 seconds
% Ys: [6,2,7,5,8,12,1,4,11,9,3,10]
