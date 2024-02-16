rule1(x,y,z) :- pred1(1,2,3), pred2(g(x),s(x),d(1)).
rule2(x,y,z) :- pred1(2,3,1), pred2(g(x),s(1),d(g(x))).
rule3(x,y,z) :- pred1(1,2,3), pred2(g(x),s(1),d(1)).
