triangle(N,T):-triangle(N,0,T).
triangle(0,T,T).
triangle(N,A,T):-N1 is N-1,A1 is A+N,triangle(N1,A1,T).


between(I,J,I):-I>=J.
between(I,J,K):-I>J,I1 is I-1,between(I1,J,K).

minimum(A,B,B):-B=<A.
minimum(A,B,A):-B>A.

min([A],A).
min([H|T],C):-min(T,Z),minimum(Z,H,C).

length_list(Xs,N):-length_list(Xs,0,N).
length_list([],N,N).
length_list([_|T],N,L):-N1 is N+1,length_list(T,N1,L).

%2
%occurence(Sub,Term,N)
occurence(Term,Term,1).
occurence(Sub,Term,0) :- Sub \= Term.
occurence_s(Sub,Term,N):- compound(Term),occurence(Sub,Term,N1),Term =.. [_|Args], subterm_list(Sub,Args,N2),N is N1+N2.
occurence_s(Sub,Term,N):-occurence(Sub,Term,N).
subterm_list(_,[],0).
subterm_list(Sub,[H|T],N) :- occurence_s(Sub,H,N1), subterm_list(Sub,T,N2), N is N1+N2.

arg_position(Term, Term, []).
arg_position(Sub, Term, P) :- compound(Term), Term =.. [_ | Args], arg_position(Sub, Args, 1, P).
arg_position(Sub, [Arg|_], N, [N|P]) :- arg_position(Sub, Arg, P).
arg_position(Sub, [_|Args], N, P) :- N1 is N + 1, arg_position(Sub, Args, N1, P).


functor1(Term,F,Arity):-Term=..[F|Args],length(Args,Arity).

arg1(N,Term,Arg):-Term=..[F|Args], position(N,Args,Arg).

%position(N,Xs,X) is true if X is the Nth element in the list Xs.
position(1,[X|Xs],X).
position(N,[X|Xs],Y):-N>1, N1 is N-1, position(N1,Xs,Y).

substitute(Old,New,Old,New).
substitute(Old,New,Term,Term):-atomic(Term), Term \= Old.
substitute(Old,New,Term,Term1):-compound(Term), Term =.. [F | OldArgs], substitute_list(Old, New, OldArgs, NewArgs), Term1 =.. [F | NewArgs].
substitute_list(Old,New,[H1|T1],[H2|T2]):-substitute(Old,New,H1,H2),substitute_list(Old,New,T1,T2).
substitute_list(_,_,[],[]).

%3
notequal(X,Y) :- X == Y, !, fail.
notequal(X,Y).

notvar(X) :- var(X), !, fail.
notvar(X).

append([X|Xs] ,Ys, [X| Zs]):-append(Xs,Ys,Zs).
append([] ,Ys,Ys).

%4
snake(L1,L2,L3):-odd(L1,L2,L3,L2,[]).

odd(_,_,[],_,_).
odd(L1,L2,[H3|L3],[],P):-print_even(P),even(L1,L2,L3,L2,[]).
odd([H1|L1],L2,L3,[_|L4],P):- append(L1,[H1],N),odd(N,L2,L3,L4,[H1|P]).

even(_,_,[],_,_).
even(L1,L2,[H3|L3],[],P):-print_odd(P),odd(L1,L2,L3,L2,[]).
even([H1|L1],L2,L3,[_|L4],P):- append(L1,[H1],N),even(N,L2,L3,L4,[H1|P]).

print_odd([]):-nl.
print_odd([H|X]):-write(H),print_odd(X).

print_even(X):-reverse(X,A),print_odd(A).

range(M,N,[M|Ns]):- M < N, M1 is M+1, range(M1,N,Ns).
range(N,N,[N]).

%5
queens(N,Qs):-range(1,N,Ns), queens(Ns, [] ,Qs).
queens(UnplacedQs,SafeQs,Qs):-select(Q,UnplacedQs,UnplacedQsl),not(attack(Q,SafeQs)),queens(UnplacedQsl, [Q|SafeQs] ,Qs).
queens([] ,Qs,Qs).

attack(X,Xs):- attack(X,1,Xs).
attack(X,N, [Y|Ys]):- X is Y+N ; X is Y-N.
attack(X,N,[Y|Ys]):- N1 is N+1, attack(X,N1,Ys).

%6
%N1,N2,N3 are equal length,i.e they are preceded by 0 whereever required.
%solve1(L1,L2,L3,carryFromRight,carryToLeft,listOfAvailableDigits, listOfUnusedDigits)
solve(N1,N2,N) :-solve1(N1,N2,N,0,0,[0,1,2,3,4,5,6,7,8,9], _).
solve1([], [], [], C,C,D,D).
solve1([D1|N1], [D2|N2], [D|N], CR, C, Digs1, Digs):-solve1(N1,N2,N, CR, CLN, Digs1, Digs2),digsum(D1,D2, CLN, D, C, Digs2, Digs).

digsum(D1,D2, C1, D, C, Digs1, Digs):-del_var(D1, Digs1, Digs2),del_var(D2, Digs2, Digs3),del_var(D,  Digs3, Digs),S is D1+D2+C1,D is S mod 10,C is S // 10.

del_var(A,L,L) :-nonvar(A), !.
del_var(A, [A|L], L).
del_var(A, [B|L], [B|L1]):-del_var(A,L,L1).


%7
/* preferences(Person, List) is true if the Person prefers people of the other sex in the order given in the List.                             */
preferences(a, [q,t,z,r,s]).  preferences(z, [e,a,d,b,c]).
preferences(b, [z,q,r,s,t]).  preferences(q, [d,e,b,a,c]).
preferences(c, [q,r,t,s,z]).  preferences(r, [a,d,b,c,e]).
preferences(d, [z,r,q,s,t]).  preferences(s, [c,b,d,a,e]).
preferences(e, [t,r,q,z,s]).  preferences(t, [d,b,c,e,a]).

/* stable(Men, Women, Marriages) is true if Marriages is a set of stable   */
/*   marriages between the Men and the Women.                              */
stable([], _, _).
stable([Man|Men], Women, Marriages):-stable_1(Women, Man, Marriages),stable(Men, Women, Marriages).

stable_1([], _, _).
stable_1([Woman|Women], Man, Marriages):-not(unstable(Man, Woman, Marriages)),stable_1(Women, Man, Marriages).

/* unstable(Man, Woman, Marriages) is true if the Man and the Woman both prefer each other to their spouses as defined by the set of Marriages.*/
unstable(Man, Woman, Marriages):-
  married(Man, Wife, Marriages),
  married(Husband, Woman, Marriages),
  prefers(Man, Woman, Wife),
  prefers(Woman, Man, Husband).
  
/* married(Man, Woman, Marriages) is true if the Man and the Woman are married as defined by the set of Marriages.                           */
married(Man, Woman, Marriages):-
  rest(m(Man, Woman), Marriages, _).  

/* prefers(Person, OtherPerson, Spouse) is true if the Person prefers the OtherPerson to his Spouse.                                            */
prefers(Person, OtherPerson, Spouse):-
  preferences(Person, Preferences),
  rest(OtherPerson, Preferences, Rest),
  rest(Spouse, Rest, _).
  
/* rest(X, Ys, Zs) is true if X is a member of the list Ys, and the list Zs is the rest of the list following X.                               */
rest(X, [X|Ys], Ys):-!.
rest(X, [_|Ys], Zs):-rest(X, Ys, Zs).


%generate_and_test(Men, Women, Marriages) is true if Marriages is a set of stable marriages between the Men and the Women.              
generate_and_test(Men, Women, Marriages):-
  generate(Men, Women, Marriages),
  stable(Men, Women, Marriages).

/* generate(Men, Women, Marriages) is true if Marriages is a set of  possible marriages between the Men and the Women.                     */
generate([], [], []).
generate([Man|Men], Women, [m(Man,Woman)|Marriages]):-
  select(Woman, Women, Women1),
  generate(Men, Women1, Marriages).

substitute(_,_,[],[]).
substitute(X,Y,[X|L1],[Y|L2]):-substitute(X,Y,L1,L2).
substitute(X,Y,[Z|L1],[Z|L2]):-substitute(X,Y,L1,L2).

%8
transform(State1, State2, Plan):-transform(State1,State2, [State1] ,Plan).
transform(State,State,Visited,[]).
transform(State1,State2,Visited,[Action|Actions]):-choose_action(Action,State1,State2),update(Action,State1,State),not(member(State,Visited)),transform(State,State2, [State|Visited] ,Actions).

choose_action(Action,State1,State2):-suggest(Action,State2), legal_action(Action,State1).
choose_action(Action,State1,State2):-legal_action(Action,State1).
suggest(to_place(X,Y,Z),State):-member(on(X,Z),State), place(Z).
suggest(to_block(X,Y,Z),State):-member(on(X,Z),State), block(Z).

legal_action(to_place(Block,Y,Place),State):-on(Block,Y,State), clear(Block,State),place(Place), clear(Place,State).
legal_action(to_block(Block1,Y,Block2),State):-on(Block1,Y,State), clear(Block1,State), block(Block2),Block1 \= Block2, clear(Block2,State).
clear(X,State):- not(member(on(A, X) ,State)).
on(X, Y ,State):- member(on(X,Y) ,State).
update(to_block(X,Y,Z),State,State1):-substitute(on(X,Y),on(X,Z),State,State1).
update(to_place(X, Y, Z), State, State1):-substitute(on(X,Y),on(X,Z),State,State1).

test_plan(Name,Plan):-initial_state(Name,I), final_state(Name,F), transform(I,F,Plan).
initial_state(test,[on(a,p),on(b,a),on(c,q),on(d,r),on(e,d)]).
final_state(test,[on(a,c),on(b,q),on(c,r),on(d,p),on(e,d)]).

block(a).
block(b).
block(c).
block(d).
block(e).
place(p).
place(q).
place(r).

%9
start([3,3,left,0,0]).
end([0,0,right,3,3]).

% is this state a legal one?
legal(CL,ML,CR,MR) :-ML>=0, CL>=0, MR>=0, CR>=0,(ML>=CL ; ML=0),(MR>=CR ; MR=0).

% Possible moves:
move([CL,ML,left,CR,MR],[CL,ML2,right,CR,MR2]):-
	% Two missionaries cross left to right.
	MR2 is MR+2,
	ML2 is ML-2,
	legal(CL,ML2,CR,MR2).

move([CL,ML,left,CR,MR],[CL2,ML,right,CR2,MR]):-
	% Two cannibals cross left to right.
	CR2 is CR+2,
	CL2 is CL-2,
	legal(CL2,ML,CR2,MR).

move([CL,ML,left,CR,MR],[CL2,ML2,right,CR2,MR2]):-
	%  One missionary and one cannibal cross left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	legal(CL2,ML2,CR2,MR2).

move([CL,ML,left,CR,MR],[CL,ML2,right,CR,MR2]):-
	% One missionary crosses left to right.
	MR2 is MR+1,
	ML2 is ML-1,
	legal(CL,ML2,CR,MR2).

move([CL,ML,left,CR,MR],[CL2,ML,right,CR2,MR]):-
	% One cannibal crosses left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	legal(CL2,ML,CR2,MR).

move([CL,ML,right,CR,MR],[CL,ML2,left,CR,MR2]):-
	% Two missionaries cross right to left.
	MR2 is MR-2,
	ML2 is ML+2,
	legal(CL,ML2,CR,MR2).

move([CL,ML,right,CR,MR],[CL2,ML,left,CR2,MR]):-
	% Two cannibals cross right to left.
	CR2 is CR-2,
	CL2 is CL+2,
	legal(CL2,ML,CR2,MR).

move([CL,ML,right,CR,MR],[CL2,ML2,left,CR2,MR2]):-
	%  One missionary and one cannibal cross right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	MR2 is MR-1,
	ML2 is ML+1,
	legal(CL2,ML2,CR2,MR2).

move([CL,ML,right,CR,MR],[CL,ML2,left,CR,MR2]):-
	% One missionary crosses right to left.
	MR2 is MR-1,
	ML2 is ML+1,
	legal(CL,ML2,CR,MR2).

move([CL,ML,right,CR,MR],[CL2,ML,left,CR2,MR]):-
	% One cannibal crosses right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	legal(CL2,ML,CR2,MR).


% Recursive call to solve the problem
path([CL1,ML1,B1,CR1,MR1],[CL2,ML2,B2,CR2,MR2],Explored,MovesList) :- 
   move([CL1,ML1,B1,CR1,MR1],[CL3,ML3,B3,CR3,MR3]), 
   not(member([CL3,ML3,B3,CR3,MR3],Explored)),
   path([CL3,ML3,B3,CR3,MR3],[CL2,ML2,B2,CR2,MR2],[[CL3,ML3,B3,CR3,MR3]|Explored],[ [[CL3,ML3,B3,CR3,MR3],[CL1,ML1,B1,CR1,MR1]] | MovesList ]).

% Solution found
path([CL,ML,B,CR,MR],[CL,ML,B,CR,MR],_,MovesList):- 
	output(MovesList).

% Printing
output([]) :- nl. 
output([[A,B]|MovesList]) :- 
	output(MovesList), 
   	write(B), write(' -> '), write(A), nl.
	
initial_state(p(left, 3, 3)).

final_state(p(right, 0, 0)).

move(p(left,  M, _), m(1, 0)):-M >= 1.
move(p(left,  _, C), m(0, 1)):-C >= 1.
move(p(left,  M, C), m(1, 1)):-M >= 1, C >= 1.
move(p(left,  M, _), m(2, 0)):-M >= 2.
move(p(left,  _, C), m(0, 2)):-C >= 2.
move(p(right, M, _), m(1, 0)):-(3 - M) >= 1.
move(p(right, _, C), m(0, 1)):-(3 - C) >= 1.
move(p(right, M, C), m(1, 1)):-(3 - M) >= 1, (3 - C) >= 1.
move(p(right, M, _), m(2, 0)):-(3 - M) >= 2.
move(p(right, _, C), m(0, 2)):-(3 - C) >= 2.

update(p(left, M0, C0), m(MB, CB), p(right, M, C)):-
  M is M0 - MB, C is C0 - CB.
update(p(right, M0, C0), m(MB, CB), p(left, M, C)):-
  M is M0 + MB, C is C0 + CB.

/* This uses an (under)estimation of the number of remaining voyages as    */
/*   the evaluation function.                                              */
value(p(_,     M, C), 1):-M + C =:= 1, !.
value(p(left,  M, C), L):-L is (M + C - 2) * 2 + 1.
value(p(right, M, C), L):-L is (M + C) * 2.

/* Ensures that, on each bank, the cannibals are not outnumbered */
legal(p(_, _, 3)):-!.
legal(p(_, _, 0)):-!.
legal(p(_, M, M)).