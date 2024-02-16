add(0,X,X).
add(X,0,X).
add(s(X),Y,Z) :- add(X,s(Y),Z).

sub(X,Y,Z) :- add(Y,Z,X).

mul(0,X,0).
mul(s(X),Y,Z) :- add(Y,Z1,Z), mul(Y,X,Z1).

div(X,Y,Z) :- mul(Y,Z,A), sub(X,A,R), grq(Y,R).

grq(0,0).
grq(s(X),0).
grq(s(X),s(Y)) :- grq(X,Y).
