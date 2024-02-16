size(empty,0).
size(l(H,T),s(S)) :- size(T,S).

append(empty, l(H,T), l(H,T)).
append(l(H1,T1), L, l(H1,T3)) :- append(T1, L, T3).
