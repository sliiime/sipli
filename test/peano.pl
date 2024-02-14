add(0,X,X).
add(X,0,X).
add(s(X),Y,Z) :- add(X,s(Y),Z).

sub(X,Y,Z) :- add(Y,Z,X).

mul(0,X,0).
mul(s(X),Y,Z) :- add(Y,Z1,Z), mul(Y,X,Z1).
