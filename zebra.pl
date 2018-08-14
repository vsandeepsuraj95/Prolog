:- use_module(library(clpfd)).
:- use_module(library(lists)).

houses([N1,N2,N3,N4,N5,C1,C2,C3,C4,C5,P1,P2,P3,P4,P5,A1,A2,A3,A4,A5,D1,D2,D3,D4,D5]):- [N1,N2,N3,N4,N5,C1,C2,C3,C4,C5,P1,P2,P3,P4,P5,A1,A2,A3,A4,A5,D1,D2,D3,D4,D5] ins 1..5,
	N1=C2,N2=A1,N3=P1,N4=D3,N5=1,D5=3,P3=D1,C1=D4,P5=A4,P2=C3,
	plusc(C1,C5,1),
	plusorminusc(A3,P4,1),
	plusorminusc(A5,P2,1),
	plusorminusc(N5,C4,1),
	all_different([N1,N2,N3,N4,N5]),
	all_different([C1,C2,C3,C4,C5]),
	all_different([P1,P2,P3,P4,P5]),
	all_different([A1,A2,A3,A4,A5]),
	all_different([D1,D2,D3,D4,D5]),
	labeling([ffc],[N1,N2,N3,N4,N5,C1,C2,C3,C4,C5,P1,P2,P3,P4,P5,A1,A2,A3,A4,A5,D1,D2,D3,D4,D5]).
	
plusc(X,Y,C):- X #= Y+C.

plusorminusc(X,Y,C):- X #= Y+C.
plusorminusc(X,Y,C):- X #= Y-C.
