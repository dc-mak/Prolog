% 6.1)
findterm(Term) :-
	read(Term),!,write(Term) ; findterm(Term).

% 6.2) Last exercise had something not(X=Y) undoing unification in failure.
% Forgot to put end_of_file in a different check and recursive call after write.
findallterm(Term) :-
	read(Term),!,check(Term,Find),! ; findallterm(Term).

check(end_of_file, _) :- !.
check(Term,Find) :- not(Term=Find),!,findallterm(Term).
check(Term,Find) :- write(Find),nl,!,findallterm(Term).

% 6.3)
comma(C) :-
	get0(N), not(N=32), !, N=C
	;
	get(M), not(M=44), put(32), M=C
	;
	put(44),get0(C).

squeeze :-
	comma(C),
	put(C),
	dorest(C).  

dorest(46) :- !.
dorest(32) :- !, get(C), put(C), dorest(C).
dorest(Letter) :- squeeze.

% 6.4)
starts(Atom, Char) :- name(Atom, [Char|_]).

% 6.5)
conc([], L, L).
conc([H|T], L, [H|R]) :- conc(T, L, R).

% Not gonna make this any more complicated.
% Remember that [s] isnae gonna work, name(s, SList).
table(Word, Plural) :-
	name(Word, Wordlist),
	name(s, SList),
	conc(Wordlist, SList, PluralList),
	name(Plural, PluralList).

% 6.6 I could have/tried to do this all in one go.
%	  Too much effort and not very readable.
getall(Words) :- get0(Char), getrest(Char,Words).

getrest(10, []) :- !.							% new line
getrest(46, ['.'|Words]) :- !, getall(Words).	% full stop
getrest(32, Words) :- !, getall(Words).			% space
getrest(L, [W|Words]) :-
	getletters(L, Lets, Nxt),
	name(W, Lets),
	getrest(Nxt,Words).

getletters(10, [], 10) :- !.	% new line
getletters(46, [], 46) :- !.	% full stop
getletters(32, [], 32) :- !.	% space
getletters(L, [L|Lets], Nxt) :-
	get0(Char), getletters(Char, Lets, Nxt).

% split into sentences.
% search for word in sentence
search_word(H, [H|_]) :- !.
search_word(H, [_|T]) :- !, search_word(H,T).

% get all sentences (prevents trailing sentences to cause syntax errors)
split([], [[]]) :- !.
split(['.'|Words], [[]|Rest]) :- !, split(Words, Rest).
split([W|Words], [[W|S]|Rest]) :- !, split(Words, [S|Rest]).

search_sen(W, [S|_], S) :- search_word(W, S).
search_sen(W, [_|T], S) :- search_sen(W, T, S).

search(W, S) :-
	getall(Words),
	split(Words, Sents),
	search_sen(W, Sents, S).
