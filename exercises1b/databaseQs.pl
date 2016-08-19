:- [databaseFacts].
% the general clause for 22S.1 to 4
crsidCollege('', '', CRSID) :- tName(CRSID), tCollege(CRSID).
crsidCollege(FullName, '', CRSID) :- tName(CRSID, FullName), tCollege(CRSID).
crsidCollege('', College, CRSID) :- tName(CRSID), tCollege(CRSID, College).
crsidCollege(FullName, College, CRSID) :- tName(CRSID, FullName), tCollege(CRSID, College).

% query for 22S.1 to 4
nameCollege(FullName, College) :- crsidCollege(FullName, College, _).

% 22S.5 Preliminary list stuff.
min(A, B, A) :- A =< B, !.
min(A, B, B) :- B =< A, !.

minList([R], R) :- !.
minList([A,B|T], R) :- min(A, B, C), minList([C|T], R). 

% 22S.5
crsidGrades(CRSID, SoFar, [Grade | Result]) :-
	tGrade(CRSID, Part, Grade),
	not((member((CRSID, Part, Grade), SoFar))), !,
	crsidGrades(CRSID, [(CRSID, Part, Grade) | SoFar], Result).
crsidGrades(_, _, []).

% 22S.5 - BAD (Answer)
minGrade(CRSID, Grade) :-
	crsidGrades(CRSID, [], AllGrades), 
	minList(AllGrades, Grade).

% 22.5 - The point
minGr(CRSID, Grade) :-
	tGrade(CRSID, _, Grade),
	not(tGrade(CRSID, _, G2), Grade < G2), !.

% Preliminary for next two questions
onlyFirsts([], []) :- !.
onlyFirsts([1|T], [1|R]) :- !, onlyFirsts(T, R).
onlyFirsts([_|T], R) :- onlyFirsts(T, R).

% 22S.6 - Use crsidGrades, but make it more general by abstracting over crsIDs.
allGrades(Visited, Result) :- 
	(tName(CRSID, _) ; tName(CRSID)),
	not(member(CRSID, Visited)), !,
	crsidGrades(CRSID, [], L),
	allGrades([CRSID|Visited], Rest),
	append(L, Rest, Result).
allGrades(_, [])

numFirsts(N) :- allGrades([], L), onlyFirsts(L, L2), length(L2, N).

% 22S.7 - Firsts per person
firstsPerson(CRSID, Num) :-
	(tName(CRSID, _) ; tName(CRSID)),
	crsidGrades(CRSID, [], L),
	onlyFirsts(L, L2),
	length(L2, Num), Num >= 1.
