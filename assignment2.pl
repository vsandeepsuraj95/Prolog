:-dynamic card/2.
adjacent(X,Y,[X,Y|_]):-!.
adjacent(X,Y,[_|Z]):-adjacent(X,Y,Z).

lst(X,[X|[]]):-!.
lst(X,[_|Y]):-lst(X,Y).

double([],[]).
double([X|Y],[X,X|Z]):-double(Y,Z).

substitute(_,_,[],[]).
substitute(X,Y,[X|L1],[Y|L2]):-substitute(X,Y,L1,L2).
substitute(X,Y,[Z|L1],[Z|L2]):-substitute(X,Y,L1,L2).

member(X,[X|_]).
member(X,[_|Ys]):-member(X,Ys).

no_doubles([],[]).
no_doubles([X|Xs],Ys):-member(X,Xs),no_doubles(Xs,Ys).
no_doubles([X|Xs],[X|Ys]):- \+member(X,Xs),no_doubles(Xs,Ys).

split_list(X,L,R):- length(X,N),N1 is round(N/2),split_list(X,N1,L,R).
split_list([],_,[],[]).
split_list(X,0,[],X).
split_list([H|X],N,[H|L],R):-L1 is N-1, split_list(X,L1,L,R).

merge([],[],[]).
merge([],R,R).
merge(L,[],L).
merge([X|L],[Y|R],[X|Z]):-X=<Y,merge(L,[Y|R],Z).
merge([X|L],[Y|R],[Y|Z]):-X>Y,merge([X|L],R,Z).

mergesort([],[]).
mergesort([X],[X]).
mergesort(X,Z):-split_list(X,L,R),mergesort(R,R1),mergesort(L,L1),merge(L1,R1,Z).

insert(X, [], [X]):- !.
insert(X, [X1|L1], [X, X1|L1]):- X=<X1, !.
insert(X, [X1|L1], [X1|L]):- insert(X, L1, L).

insertionsort([], []):- !.
insertionsort([H|T], S):- insertionsort(T, S1), insert(H, S1, S).

median([],[]).
median([X1],[X1]).
median([X1,X2],[M]):-insertionsort([X1,X2],[M|_]).
median([X1,X2,X3],[M]):-insertionsort([X1,X2,X3],[_,M|_]).
median([X1,X2,X3,X4],[M]):-insertionsort([X1,X2,X3,X4],[_,M|_]).
median([X1,X2,X3,X4,X5|R],[M|Ms]):-insertionsort([X1,X2,X3,X4,X5],[_,_,M|_]),median(R,Ms).

median_of_medians([M|[]],M):-!.
median_of_medians([M1|Xs],M):-Xs\=[],median([M1|Xs],Ms),median_of_medians(Ms,M).

partition([],_,[],[]).
partition([X|Xs],Y,[X|Ls],Bs):-X=<Y,!,partition(Xs,Y,Ls,Bs).
partition([X|Xs],Y,Ls,[X|Bs]):-X>Y,partition(Xs,Y,Ls,Bs).

kth_largest([X],_,X).
kth_largest(Xs,K,A):- median_of_medians(Xs,M),partition(Xs,M,Ls,Bs),length(Ls,L),K>L,K1 is K-L,kth_largest(Bs,K1,A).
kth_largest(Xs,K,A):- median_of_medians(Xs,M),partition(Xs,M,Ls,_),length(Ls,L),K=<L,kth_largest(Ls,K,A).

%poker
successor(straight_flush, four_of_a_kind).
successor(four_of_a_kind, full_house).
successor(full_house, straight).
successor(straight, flush).               
successor(flush, three_of_a_kind).
successor(three_of_a_kind, two_pair).     
successor(two_pair, one_pair).
successor(one_pair, no_pair).

successor(ace,king).
successor(king,queen).
successor(queen,jack).
successor(jack,10).      
successor(10,9).         
successor(9,8).
successor(8,7).          
successor(7,6).          
successor(6,5).
successor(5,4).          
successor(4,3).          
successor(3,2).

greater_value(A,B) :- successor(A,X) , (X = B ; greater_value(X,B)).

%card(5,spades) repersents 5 of spades
put(card(X,Y), [], [card(X,Y)]):-!.
put(card(X,Y), [card(X1,Z)|L1], [card(X,Y),card(X1,Z)|L1]):- greater_value(X1,X), !.
put(card(X,Y), [card(X1,Z)|L1], [card(X1,Z)|L]):- put(card(X,Y), L1, L).
sort_hand([],[]):-!.
sort_hand([X|Xs],S):- sort_hand(Xs,S1), put(X,S1,S).

determine_hand([card(A,X),card(B,X),card(C,X),card(D,X),card(E,X)], straight_flush,E,ace,ace) :- successor(E,D), successor(D,C), successor(C,B), successor(B,A),!.
determine_hand([card(A,_),card(B,_),card(B,_),card(B,_),card(C,_)], four_of_a_kind,B,ace,ace) :- A = B ; C = B,!.
determine_hand([card(A,_),card(A,_),card(A,_),card(B,_),card(B,_)], full_house,A,B,ace) :-  !.
determine_hand([card(A,_),card(A,_),card(B,_),card(B,_),card(B,_)], full_house,B,A,ace) :-  !.
determine_hand([card(_,X),card(_,X),card(_,X),card(_,X),card(A,X)], flush,A,ace,ace) :- !.
determine_hand([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)], straight,E,ace,ace) :-  successor(E,D), successor(D,C), successor(C,B), successor(B,A), !.
determine_hand([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)], three_of_a_kind,B,E,D) :-  (A = B, B = C), !.
determine_hand([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)], three_of_a_kind,B,E,A) :-  (B = C, C = D), !.
determine_hand([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)], three_of_a_kind,C,B,A) :-  (C = D, D = E) , !.
determine_hand([card(A,_),card(A,_),card(B,_),card(B,_),card(C,_)], two_pair,B,A,C) :- !.
determine_hand([card(A,_),card(A,_),card(C,_),card(B,_),card(B,_)], two_pair,B,A,C) :- !.
determine_hand([card(C,_),card(A,_),card(A,_),card(B,_),card(B,_)], two_pair,B,A,C) :- !.
determine_hand([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)], one_pair,X,Y,Z) :-(A = B,X = B,Y = E,Z = D; B = C,X = B,Y = E,Z = D ; C = D, X = D,Y = E,Z = B; D = E,X = D,Y = C,Z = B), !.
determine_hand([card(_,_),card(_,_),card(C,_),card(D,_),card(E,_)],no_pair,E,D,C).

better_hand(H1,H2,H):- sort_hand(H1,Sh1),sort_hand(H2,Sh2),determine_hand(Sh1,K1,X1,Y1,Z1),determine_hand(Sh2,K2,X2,Y2,Z2),win(H1,K1,X1,Y1,Z1,H2,K2,X2,Y2,Z2,H).

win(Sh1,K1,X1,Y1,Z1,Sh2,K2,X2,Y2,Z2,W):-greater_value(K1,K2),W=Sh1,!;
	greater_value(K2,K1),W=Sh2,!;
	K1=K2,greater_value(X1,X2),W=Sh1,!;
	K1=K2,greater_value(X2,X1),W=Sh2,!;
	K1=K2,X1=X2,greater_value(Y1,Y2),W=Sh1,!;
	K1=K2,X1=X2,greater_value(Y2,Y1),W=Sh2,!;
	K1=K2,X1=X2,Y1=Y2,greater_value(Z1,Z2),W=Sh1,!;
	K1=K2,X1=X2,Y1=Y2,greater_value(Z2,Z1),W=Sh2,!;
	W=tie;

plus(0,0,0).
plus(0,Y, Y).
plus(s(X),Y,s(Z)):-plus(X,Y,Z).
greaterthan(s(X),0).
greaterthan(s(X),s(Y)):-greaterthan(X,Y).
lessthan(0,s(X)).
lessthan(s(X),s(Y)):-lessthan(X,Y).

sum_tree(nil,0).
sum_tree(node(X,nil,nil),X).
sum_tree(node(X,L,R),Sum):-sum_tree(L,L_sum),sum_tree(R,R_sum),plus(L_sum, R_sum, Z),plus(X,Z,Sum).

% min_member(T, Min) :- Min is the minimum element in SBT T.
max_member(node(X, _, nil), X).
max_member(node(_, _, Right), MaxRight) :- max_member(Right, MaxRight).

% delete_node(E, T, Tn) :- delete element E from SBT T to obtain SBT Tn.
delete_node(_, nil, nil).
delete_node(X, node(X, nil, nil), nil).
delete_node(X, node(X, Left, nil), Left).
delete_node(X, node(X, nil, Right), Right).
delete_node(X, node(X, Left, Right), node(MaxLeft, LeftReduced, Right)) :- max_member(Left, MaxLeft), delete_node(MaxLeft, Left, LeftReduced).
delete_node(E, node(X, Left, Right), node(X, LeftReduced, Right)) :- lessthan(E,X), delete_node(E, Left, LeftReduced).
delete_node(E, node(X, Left, Right), node(X, Left, RightReduced)) :- greaterthan(E,X), delete_node(E, Right, RightReduced).



triangle(N,T):-triangle(N,0,T).
triangle(0,T,T).
triangle(N,A,T):-N1 is N-1,A1 is A+N,triangle(N1,A1,T).

range(J,J,[]).
range(I,J,[I|T]):-I>J,I1 is I-1,range(I1,J,T).

minimum(A,B,B):-B=<A.
minimum(A,B,A):-B>A.

min([A],A).
min([H|T],C):-min(T,Z),minimum(Z,H,C).

length_list(Xs,N):-length_list(Xs,0,N).
length_list([],N,N).
length_list([_|T],N,L):-N1 is N+1,length_list(T,N1,L).
