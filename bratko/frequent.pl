% Library of frequently used predicates

:- op( 900, fy, not).

% not Goal): negation as failure; 
%   Note: This is often available as a built-in predicate,
%   often written as prefix operator "\+", e.g. \+ likes(mary,snakes)

not Goal  :-
  Goal, !, fail
  ; 
  true.

% once( Goal):
%   Produce one solution of Goal only (only the first solution)

once( Goal)  :-
  Goal, !.

% member( X, List): X is a member of List

member(X,[X | _]).                 % X is head of list

member( X, [_ | Rest])  :-         
  member( X, Rest).                % X is in body of list

%  conc(L1,L2,L3): list L3 is th econcatenation of lists L1 and L2

conc( [], L, L).

conc( [X | L1], L2, [X | L3])  :-
  conc( L1, L2, L3).

% del(X,L0,L): List L is equal to list L0 with X deleted
%   Note: Only one occurrence of X is deleted

del( X, [X | Rest], Rest).        % Delete the head

del( X, [Y | Rest0], [Y | Rest])  :-
  del( X, Rest0, Rest).

%  subset( Set, Subset):  list Set contains all the elements of list Subset 
%    Note: The elements of Subset appear in Set in the same order as in Subset

subset( [], []).

subset( [First | Rest], [First | Sub])  :-       % Retain First in subset
  subset( Rest, Sub).

subset( [First | Rest], Sub)  :-                 % Remove First
  subset( Rest, Sub).

%  set_difference( Set1, Set2, Set3):  Set3 is the list representing 
%    the difference of sets represented by lists Set1 and Set2

set_difference( [], _, []).

set_difference( [X | S1], S2, S3)  :-
  member( X, S2),  !,                            % X in set S2
  set_difference( S1, S2, S3).

set_difference( [X | S1], S2, [X | S3])  :-      % X not in S2
  set_difference( S1, S2, S3).

%  length( List, Length): Lentgh is the length of List
%    Note: Often provided as built-in predicate
%    The definition below is tail-recursive 
%    It can also be used to generate efficiently list of given length

length( L, N)  :-
  length( L, 0, N).

length( [], N, N).

length( [_ | L], N0, N)  :-
  N1 is N0 + 1,
  length( L, N1, N).



%  max( X, Y, Max): Max = max(X,Y)

max( X, Y, Max)  :-
  X >= Y, !, Max = X
  ;
  Max = Y.

%  min( X, Y, Min): Min = min(X,Y) 

min( X, Y, Min)  :-
  X =< Y, !, Min = X
  ;
  Min = Y.


% copy_term( T1, T2): T2 is equal to T1 with variables renamed
% This is often available as a built-in predicate
% Procedure below assumes that copy_term is called with T2 uninstantiated

copy_term( Term, Copy)  :-
  asserta( term_to_copy( Term)),
  retract( term_to_copy( Copy)), !.
