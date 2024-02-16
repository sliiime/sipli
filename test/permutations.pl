perm(empty,empty).
perm(l(H,T), l(X,T1)) :- delete(l(H,T), X, L), perm(L,T1).

size(empty,0).
size(l(H,T),s(S)) :- size(T,S).

delete(empty,empty).
delete(l(X,T),X,T).
delete(l(X,T),Y,l(X,T1)) :- delete(T, Y, T1).

append(empty, l(H,T), l(H,T)).
append(l(H1,T1), L, l(H1,T3)) :- append(T1, L, T3).
