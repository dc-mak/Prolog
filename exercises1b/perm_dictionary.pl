:- [perm].
:- [anagram].

anagram(L, R) :- perm(L, R), word(R).
anagram2(L, R) :- word(R), perm(R, L).
