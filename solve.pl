:- use_module(library(clpfd)).
:- use_module(library(lists)).


union([H|X], Y, [H|Z]) :- not(member(H,Y)), 
    union(X,Y,Z).    
union([H|X], Y, Z) :- member(H,Y), 
    union(X,Y,Z).    
    
union([],X,X).

solve([Q,W,E,R],[A,S,D,F],[Z,X,C,V]):-

	union([Q,W,E,R],[A,S,D,F],L3),union(L3,[Z,X,C,V],U),
	all_different(U),
	U ins 0..9,
	
	1000*Q + 100*W + 10*E + R + 1000*A + 100*S + 10*D + F #=1000*Z + 100*X + 10*C + V,
	labeling([ffc],U).
	
%inspired from https://en.wikibooks.org/wiki/Prolog/Constraint_Logic_Programming
	
	