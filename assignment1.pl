mother(lisa, abe).
mother(lisa, sarah).
mother(nancy, john).
mother(sarah, susan).
mother(mary, jill).
mother(susan, jack).
mother(susan, phil).
mother(kim, jim).

father(tony, abe).
father(tony, sarah).
father(abe, john).
father(bill, susan).
father(john, jill).
father(rob, jack).
father(rob, phil).
father(jack,jim).

female(lisa).
female(nancy).
female(sarah).
female(mary).
female(susan).
female(jill).
female(kim).
female(ann).
female(martha).

male(tony).
male(abe).
male(bill).
male(john).
male(rob).
male(jack).
male(phil).
male(rick).
male(jim).

married(lisa,tony).
married(tony,lisa).
married(nancy,abe).
married(abe,nancy).
married(sarah,bill).
married(bill,sarah).
married(mary,john).
married(john,mary).
married(susan,rob).
married(rob,susan).
married(jill,rick).
married(rick,jill).
married(kim,jack).
married(jack,kim).
married(phil,ann).
married(ann,phil).
married(jim,martha).
married(martha,jim).

parent(X,Y):-father(X,Y).
parent(X,Y):-mother(X,Y).

sibling(X,Y):-father(A,X),father(A,Y),mother(B,X),mother(B,X), X\=Y.

fcousin(X,Y):-parent(A,X),parent(B,Y),sibling(A,B).

scousin(X,Y):-parent(A,X),parent(B,Y),fcousin(A,B).

nephew(X,Y):-male(X),parent(Z,X),sibling(Z,Y).
nephew(X,Y):-male(X),parent(Z,X),sibling(Z,V),married(V,Y).
nephew(X,Y):-male(X),married(X,Z),parent(V,Z),sibling(V,Y).
nephew(X,Y):-male(X),married(X,Z),parent(Z1,Z),sibling(Z1,V),married(V,Y).

niece(X,Y):-female(X),parent(Z,X),sibling(Z,Y).
niece(X,Y):-female(X),parent(Z,X),sibling(Z,V),married(V,Y).
niece(X,Y):-female(X),married(X,Z),parent(Z1,Z),sibling(Z1,Y).
niece(X,Y):-female(X),married(X,Z),parent(Z1,Z),sibling(Z1,V),married(V,Y).

grnephew(X,Y):-male(X),parent(Z,X),nephew(Z,Y).
grnephew(X,Y):-male(X),parent(Z,X),niece(Z,Y).

anc(X,Y):-parent(X,Y).
anc(X,Y):-parent(X,Z),anc(Z,Y).

manc(X,Y):-male(X),anc(X,Y).

samegencousins(X,Y):-fcousin(X,Y).
samegencousins(X,Y):-parent(P1,X),parent(P2,Y),samegencousins(P1,P2).

%plus(A,B,C):A+B=C
plus(0,Y, Y).
plus(s(X),Y,s(Z)):-plus(X,Y,Z).

%times(A,B,C):A*B=C
times(0,Y,0).
times(s(X),Y,A):-plus(Z,Y,A),times(X,Y,Z).

%greaterthan(X,Y):X>Y
greaterthan(s(X),0).
greaterthan(s(X),s(Y)):-greaterthan(X,Y).

%fact(X,Y):X!=Y
fact(0,s(0)).
fact(s(X),B):-times(Z,s(X),B),fact(X,Z).

%divide(X,Y,Q,R): Y*Q+R=X
divide(X, Y, 0, X):- greaterthan(Y,X).
divide(X,X,s(0),0).
divide(X,Y,s(Q),R):-greaterthan(X,Y),plus(Y,Z,X),divide(Z,Y,Q,R). 

fib(0,0).
fib(s(0),s(0)).
fib(s(s(X)),A):-fib(X,B),fib(s(X),Z),plus(Z,B,A).
