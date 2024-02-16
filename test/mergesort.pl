cut(X,0,empty,X).
cut(l(H,T),s(X),l(H,T1),R) :- cut(T,X,T1,R).
halfcut(A,L,R) :- size(A,S), div(S,s(s(0)),M), cut(A,M,L,R).

merge(empty,R,R).
merge(L,empty,L).
merge(l(H1,T1), l(H2,T2), l(H1,Z)) :- grq(H2,H1), merge(T1, l(H2,T2), Z).
merge(l(H1,T1), l(H2,T2), l(H2,Z)) :- grq(H1,H2), merge(l(H1,T1), T2, Z).

mergesort(l(H,empty),l(H,empty)).
mergesort(A0,A1) :- halfcut(A0,L0,R0), mergesort(L0,L1), mergesort(R0,R1), merge(L1,R1,A1).

add(0,X,X).
add(X,0,X).
add(s(X),Y,Z) :- add(X,s(Y),Z).

sub(X,Y,Z) :- add(Y,Z,X).

mul(0,X,0).
mul(s(X),Y,Z) :- add(Y,Z1,Z), mul(Y,X,Z1).

div(X,Y,Z) :- mul(Y,Z,A), add(A,R,X), gr(Y,R).

gr(s(X),0).
gr(s(X),s(Y)) :- gr(X,Y).

grq(0,0).
grq(s(X),0).
grq(s(X),s(Y)) :- grq(X,Y).

size(empty,0).
size(l(H,T),s(S)) :- size(T,S).

append(empty, l(H,T), l(H,T)).
append(l(H1,T1), L, l(H1,T3)) :- append(T1, L, T3).
