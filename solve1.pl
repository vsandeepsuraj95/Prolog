combine([Head|Tail], INPUT, [Head|OUTPUT]) :- not(member(Head,INPUT)), 
    combine(Tail,INPUT,OUTPUT).    
combine([Head|Tail], INPUT, OUTPUT) :- member(Head,INPUT), 
    combine(Tail,INPUT,OUTPUT).    
    
combine([],[Head|Tail],[Head|Tail]).
combine([Head|Tail],[],[Head|Tail]).

cryptarithmetic([P1,Q1,R1,S1] + [P2,Q2,R2,S2] = [P,Q,R,S]) :- 
   combine([P1,Q1,R1,S1], [P2,Q2,R2, S2], L), combine(L, [P,Q,R,S], RAND),
   RAND ins 0..9, 
   all_different(RAND),
   P1*1000 + Q1*100 + R1*10 + S1 
   + P2*1000 + Q2*100 + R2*10 + S2 
   #= P*1000 + Q*100 + R*10 + S, 
   
   label(RAND).