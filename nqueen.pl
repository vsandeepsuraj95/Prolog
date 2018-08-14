:- use_module(library(clpfd)).

nqueen(N, Qs) :-
        length(Qs, N),
        Qs ins 1..N,
        safe(Qs),
	labeling([ffc],Qs).

notattack(X,Xs) :- notattack(X,Xs,1).
notattack(X,[],N).
notattack(X,[Y|Ys],N) :- X #\= Y, 
			 X #\= Y - N, 
			 X #\= Y + N,
			 N1 is N + 1,
			 notattack(X,Ys,N1).

safe([]).
safe([F|T]) :- notattack(F,T), safe(T).

%inspired from professors notes.
