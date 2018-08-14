plus(0,Y, Y).
plus(s(X),Y,s(Z)):-plus(X,Y,Z).

times(0,Y,0).
times(s(X),Y,A):-times(X,Y,Z),plus(Z,Y,A).

greaterthan(s(X),0).
greaterthan(s(X),s(Y)):-greaterthan(X,Y).

fact(0,s(0)).
fact(s(X),B):-fact(X,Z),times(Z,s(X),B).

divide(X, Y, 0, X):- greaterthan(Y,X).
divide(X,X,s(0),0).
divide(X,Y,s(Q),R):-greaterthan(X,Y),plus(Y,Z,X),divide(Z,Y,Q,R). 

fib(0,0).
fib(s(0),s(0)).
fib(s(s(X)),A):-fib(X,B),fib(s(X),Z),plus(Z,B,A).

append([],Y,Y).
append([H|T],Y,[H|R]):-append(T,Y,R).

rev(L,R):-rev(L,[],R).
rev([],R,R).
rev([H|T],P,R):-rev(T,[H|P],R).