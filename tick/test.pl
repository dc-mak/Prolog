use_module(library(time)).
use_module(library(lists)).

test_harness__test_exceeded :- print('Timeout exceeded\n'),!, fail.

test_harness__call(X) :-
	catch(call_with_time_limit(5000,X),
			time_limit_exceeded,
			test_harness__test_exceeded).

test_harness__test_check(_,[]).
test_harness__test_check(Puzzle,[H|T]) :-
	format('Check whether we can test the solution ~p\n',[H]),
	test_harness__call(puzzle(Puzzle,H)), !,
	test_harness__test_check(Puzzle,T).

test_harness__test(Puzzle,Solutions) :-
	lists:length(Solutions,Len),
	format('Testing with ~p\n',[Puzzle]),
	test_harness__test_check(Puzzle,Solutions),
	format('Generating solutions....expecting ~p solutions\n',Len),
	test_harness__call(findall(S,puzzle(Puzzle,S),Ss)),!,
	length(Ss,SLen),
	format('Found ~p solutions\n',SLen),
	format('Solutions:\n~p\n',[Ss]),
	lists:permutation(Ss,Solutions),
	print('PASS!\n').

test_harness__test1 :-
    P0 = ['74', [[1,1,0,0,1,0], [0,1,0,1,0,0], [0,1,0,0,1,0], [0,1,0,0,1,1]]],
    P1 = ['65', [[1,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,1,0,1]]],
    P2 = ['13', [[0,1,0,1,0,1], [1,1,0,1,0,1], [1,1,0,0,1,1], [1,1,0,0,1,0]]],
    P3 = ['Cc', [[0,0,1,1,0,0], [0,0,1,1,0,0], [0,1,0,0,1,0], [0,0,1,1,0,0]]],
    P4 = ['98', [[1,1,0,0,1,0], [0,1,0,0,1,0], [0,0,1,1,0,0], [0,0,1,1,0,1]]],
    P5 = ['02', [[0,0,1,1,0,0], [0,1,0,0,1,1], [1,0,1,1,0,0], [0,0,1,1,0,0]]],
    test_harness__test([P0,P1,P2,P3,P4,P5],[
	     [[P0, 0], [P1, 2], [P4, 3], [P2, 1], [P3, 0], [P5, 2]],
	     [[P0, 0], [P1, 2], [P4, 3], [P2, 1], [P3,-4], [P5, 2]],
	     [[P0, 0], [P1, 2], [P4, 3], [P2, 1], [P5, 3], [P3, 3]],
	     [[P0, 0], [P1, 2], [P4, 3], [P2, 1], [P5, 3], [P3,-3]]]).
    
:- test_harness__test1.
