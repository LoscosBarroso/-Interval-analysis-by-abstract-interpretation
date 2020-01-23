:-module(_,_,[]).

:-use_module(library(lists)).

%Values for K
k(K):- K = [-5000,-2000,-1000,-500,-200,-100,-50,-20,-10,-5,-2,-1,0,1,2,5,10,20,50,100,200,500,1000,2000,5000].

%1. The interval data type
i('-inf',_).
i(_,'inf').
i(X,Y):- number(X), number(Y),  Y >= X.

uni('mt','mt','mt').
uni(i(X1,Y1),i(X2,Y2),i(X,Y)):-
    i(X1,Y1), i(X2,Y2),
    max(Y1,Y2,Y), min(X1,X2,X),
    i(X,Y).
uni('mt',i(X,Y),i(X,Y)):-
    i(X,Y).
uni(i(X,Y),'mt',i(X,Y)):-
    i(X,Y).

inter(i(X1,Y1),i(X2,Y2),i(X,Y)):-
    i(X1,Y1), i(X2,Y2),
    min(Y1,Y2,Y), max(X1,X2,X),
    i(X,Y).
inter(i(X1,Y1),i(X2,Y2),'mt'):-
    i(X1,Y1), i(X2,Y2).
inter('mt',_,'mt').
inter(_,'mt','mt').

%2. Abstract interpreter for expressions
nExp2inter(numVar(Name),VarIntList,Interval):-
    member(vVal(Name,Interval),VarIntList),!.

nExp2inter(num(N),_,i(N,N)).

nExp2inter(neg(NumExp),VarIntList,i(NegB,NegA)):-
    nExp2inter(NumExp,VarIntList,i(A,B)),
    negate(A,NegA),
    negate(B,NegB).

nExp2inter(div(NumExp,NumExpDiv),VarIntList,i('-inf','inf')):-
    nExp2inter(NumExpDiv,VarIntList,i(B1,B2)),
    B is B1*B2,
    min(B,0,B),
    nExp2inter(NumExp,VarIntList,i(_,_)).

nExp2inter(div(NumExp,NumExpDiv),VarIntList,i(Min,Max)):-
    nExp2inter(NumExpDiv,VarIntList,i(B1,B2)),
    B is B1*B2,
    min(B,0,0),
    nExp2inter(NumExp,VarIntList,i(A1,A2)),
    divide(A1,B1,M1),
    divide(A1,B2,M2),
    divide(A2,B1,M3),
    divide(A2,B2,M4),
    margin(M1,M2,M3,M4,Min,Max).

nExp2inter(times(NumExp1,NumExp2),VarIntList,i(Min,Max)):-
    nExp2inter(NumExp1,VarIntList,i(A1,A2)),
    nExp2inter(NumExp2,VarIntList,i(B1,B2)),
    multiply(A1,B1,M1),
    multiply(A1,B2,M2),
    multiply(A2,B1,M3),
    multiply(A2,B2,M4),
    margin(M1,M2,M3,M4,Min,Max).

nExp2inter(sub(NumExp1,NumExp2),VarIntList,i(Min,Max)):-
    nExp2inter(NumExp1,VarIntList,i(A1,A2)),
    nExp2inter(NumExp2,VarIntList,i(B1,B2)),
    minsubstract(A1,B2,Min),
    maxsubstract(A2,B1,Max).

nExp2inter(plus(NumExp1,NumExp2),VarIntList,i(Min,Max)):-
    nExp2inter(NumExp1,VarIntList,i(A1,A2)),
    nExp2inter(NumExp2,VarIntList,i(B1,B2)),
    minadd(A1,B1,Min),
    maxadd(A2,B2,Max).
nExp2inter(_,_,'mt').

%3. Abstract transfer function
abstractTransfer('skip',VarIntList,VarIntList).
abstractTransfer(cond(_),VarIntList,VarIntList).
abstractTransfer(assB(_,_),VarIntList,VarIntList).
abstractTransfer(assN(numVar(Name),numExp(NumExp)),VarIntList,[vVal(Name,Interval)|VarIntList1]):-
    member(vVal(Name,I),VarIntList),!,
    nExp2inter(NumExp,VarIntList,Interval),
    delete(VarIntList, vVal(Name,I), VarIntList1).
abstractTransfer(assN(numVar(Name),numExp(NumExp)),VarIntList,[vVal(Name,Interval)|VarIntList]):-
    nExp2inter(NumExp,VarIntList,Interval).

%4. Kleene's Asacending Chain
%
%For this que need to include aditional information in our CFG:
%
%    -VarIntList (List of intervals for each variable) needs to be added to every block.
%    -VarWideIntList (List of widened intervals for each variable) needs to be added to every block.
%
%After that is added we can begin with the implementation of the algorithm.
%Note that we only keep track of the interval values at the entranace of a CFG block, not at the exit. The exit values are easily obtainable via the abstract interpretation function and therefore omited.

kleenitialize([],[]).

kleenitialize([b(N,Block,Next)|CFG],[b(N,Block,[],Next)|CFGV]):-
    kleenitialize(CFG,CFGV).

kleeniteration(CFGV,CFGV,K,K,K). %stop if iteration limit is reached.
kleeniteration(CFGV,CFGV2,N,TotalIterations,Limit):-
    iteration(CFGV,CFGV,CFGV1),
    N1 is N+1,
    kleeniteration2(CFGV,CFGV1,CFGV2,N1,TotalIterations,Limit). 


kleeniteration2(CFGV,CFGV1,CFGV1,K,K,_):- equal_wide_lists(CFGV,CFGV1),!. %stop if a fixpoint is reached.
kleeniteration2(_,CFGV1,CFGV2,N,TotalIterations,Limit):- kleeniteration(CFGV1,CFGV2,N,TotalIterations,Limit).


iteration(Ref,[],[b(0,Block,OldVarIntList,NextBlocks)]):- %The first block added to the next state is the intial one.
    member(b(0,Block,OldVarIntList,NextBlocks),Ref).
iteration(Ref,[b(_,Block,VarIntList,NextBlocks)|CFG],CFGV1):-
    iteration(Ref,CFG,CFGV), %Iterating over all the blocks
    abstractTransfer(Block,VarIntList,VarIntList1), %we compute their abstract transer function value
    update(CFGV,Ref,NextBlocks,VarIntList1,CFGV1). %and add the result to the next state as inputs for their subsequent blocks.

update(CFGV,_,[],_,CFGV).
update(CFGV,Ref,[N|Ns],VarIntList,CFGV2):-
    member(b(N,Block,OldVarIntList,NextBlocks),CFGV),!, %Not the first time adding to that block to the enxt state.
    delete(CFGV,b(N,Block,OldVarIntList,NextBlocks),CFGV1), %Delete old input intervals
    updateVarInts(OldVarIntList,VarIntList,NewVarIntList), %Computing the union of input intervals.
    update([b(N,Block,NewVarIntList,NextBlocks)|CFGV1],Ref,Ns,VarIntList,CFGV2). %adding the new input intervals to the next state.
update(CFGV,Ref,[N|Ns],VarIntList,CFGV1):-%otherwise it is the first time we include this block in the next state and we take the information from the reference value.
    member(b(N,Block,_,NextBlocks),Ref),!,
    update([b(N,Block,VarIntList,NextBlocks)|CFGV],Ref,Ns,VarIntList,CFGV1).

updateVarInts([],[],[]).
updateVarInts(VarIntList,[],VarIntList).%this is not needed but may speed things up.
updateVarInts([],VarIntList,VarIntList).%after we have parsed all the variables that appear i nthe first list, we cope thos of the second list as they are (the shared one have already been pruned)
updateVarInts([vVal(Name,I1)|OldVarIntList],VarIntList,[vVal(Name,I3)|NewVarIntList]):-
    member(vVal(Name,I2),VarIntList),!, %if both variable-interval lists share a variable we add the union.
    delete(VarIntList,vVal(Name,I2),VarIntList1), %(after deleting the old value)
    updateVarInts(OldVarIntList,VarIntList1,NewVarIntList),
    uni(I1,I2,I3).
updateVarInts([vVal(Name,I1)|OldVarIntList],VarIntList,[vVal(Name,I2)|NewVarIntList]):-
    updateVarInts(OldVarIntList,VarIntList,NewVarIntList), %if only the first one has it, we just copy it.
    uni('mt',I1,I2).

%5. Widening function
w('mt','mt','mt').
w(i(WX,WY),'mt',i(WX,WY)):-
    k(Ks),
    member(WX,['inf','-inf'|Ks]),
    member(WY,['inf','-inf'|Ks]).
w('mt',i(X,Y),i(WX,WY)):-
    i(X,Y),
    k(Ks),
    max_low(X,Ks,WX,'-inf'),
    min_high(Y,Ks,WY).
w(i(WX,WY),i(X,Y),i(WX1,WY1)):-
    k(Ks),
    member(WX,['inf','-inf'|Ks]),
    member(WY,['inf','-inf'|Ks]),
    i(X,Y),
    min(WX,X,A),
    max(WY,Y,B),
    max_low(A,Ks,WX1,'-inf'),
    min_high(B,Ks,WY1).

kleenitializew([],[]).

kleenitializew([b(N,Block,Next)|CFG],[b(N,Block,[],[],Next)|CFGV]):- %For this analysis we need to keep track of the regular (R) and the wide (W) interval states
    kleenitializew(CFG,CFGV).

kleeniterationw(CFGV,CFGV3,N,Count):- % CFGV is at state R1,W1
    iterationw(CFGV,CFGV,CFGV1), % CFGV1 is at state R2 = F(W1),W1
    widening(CFGV1,CFGV2), % CFGV2 is at state R2,W2 = widening(R2,W1)
    N1 is N+1,
    kleeniterationw2(CFGV,CFGV2,CFGV3,N1,Count).

kleeniterationw2(CFGV,CFGV1,CFGV1,N,N):- equal_wide_lists(CFGV,CFGV1),!. %We only stop if a fixpoint is reached
kleeniterationw2(_,CFGV1,CFGV2,N,Count):- kleeniterationw(CFGV1,CFGV2,N,Count).


iterationw(Ref,[],[b(0,Block,OldVarIntList,VarWideIntList,NextBlocks)]):-
    member(b(0,Block,OldVarIntList,VarWideIntList,NextBlocks),Ref).
iterationw(Ref,[b(_,Block,_,VarWideIntList,NextBlocks)|CFG],CFGV1):-
    iterationw(Ref,CFG,CFGV),
    abstractTransfer(Block,VarWideIntList,VarIntList1),
    updatew(CFGV,Ref,NextBlocks,VarIntList1,CFGV1).

updatew(CFGV,_,[],_,CFGV).
updatew(CFGV,Ref,[N|Ns],VarIntList,CFGV2):-
    member(b(N,Block,OldVarIntList,VarWideIntList,NextBlocks),CFGV),!, %Not the first time adding to that node.
    delete(CFGV,b(N,Block,OldVarIntList,VarWideIntList,NextBlocks),CFGV1), %Delete old input intervals
    updateVarInts(OldVarIntList,VarIntList,NewVarIntList), %Computing the union of input intervals.
    updatew([b(N,Block,NewVarIntList,VarWideIntList,NextBlocks)|CFGV1],Ref,Ns,VarIntList,CFGV2).
updatew(CFGV,Ref,[N|Ns],VarIntList,CFGV1):-
    member(b(N,Block,_,VarWideIntList,NextBlocks),Ref),!,
    updatew([b(N,Block,VarIntList,VarWideIntList,NextBlocks)|CFGV],Ref,Ns,VarIntList,CFGV1).



%Auxiliary functions.
max('inf',_,'inf').
max('-inf',X,X).
max(_,'inf','inf').
max(X,'-inf',X).
max(A,B,A):- A > B,!.
max(A,B,B):- B >= A.

min('-inf',_,'-inf').
min('inf',X,X).
min(_,'-inf','-inf').
min(X,'inf',X).
min(A,B,B):- A > B,!.
min(A,B,A):- B >= A.

negate('inf','-inf').
negate('-inf','inf').
negate(A,NA):- NA is -A.

divide('inf','inf','inf').
divide('inf','-inf','-inf').
divide('inf',X,'inf'):-X >= 0,!.
divide('inf',X,'-inf'):- X < 0.
divide('-inf','inf','-inf').
divide('-inf','-inf','inf').
divide('-inf',X,'-inf'):- X >= 0,!.
divide('-inf',X,'inf'):- X < 0.
divide(0,X,0):- X =\= 0.
divide(_,'inf',0).
divide(_,'-inf',0).
divide(A,B,D):- B =\= 0,!, D is A/B.
divide(X,0,'inf'):- X > 0,!.
divide(X,0,'-inf'):- X < 0.

multiply('inf','inf','inf').
multiply('inf','-inf','-inf').
multiply('inf',0,0).
multiply('inf',X,'inf'):- X > 0,!.
multiply('inf',X,'-inf'):- X < 0.
multiply('-inf','inf','-inf').
multiply('-inf','-inf','inf').
multiply('-inf',0,0).
multiply('-inf',X,'-inf'):- X > 0,!.
multiply('-inf',X,'inf'):- X < 0.
multiply(0,_,0).
multiply(_,0,0).
multiply(X,'-inf','-inf'):- X > 0,!.
multiply(X,'-inf','inf'):- X < 0.
multiply(X,'inf','inf'):- X > 0,!.
multiply(X,'inf','-inf'):- X < 0.
multiply(A,B,D):- D is A*B.

margin(M1,M2,M3,M4,Min,Max):-
    max(M1,M2,Max12),
    max(M3,M4,Max34),
    max(Max12,Max34,Max),
    min(M1,M2,Min12),
    min(M3,M4,Min34),
    min(Min12,Min34,Min).

minsubstract('-inf',_,'-inf').
minsubstract(_,'inf','-inf').
minsubstract('inf',_,'inf').
minsubstract(_,'-inf','inf').
minsubstract(A,B,Min):- Min is A-B.

maxsubstract('inf',_,'inf').
maxsubstract(_,'-inf','inf').
maxsubstract('-inf',_,'-inf').
maxsubstract(_,'inf','-inf').
maxsubstract(A,B,Max):- Max is A-B.

minadd('-inf',_,'-inf').
minadd(_,'-inf','-inf').
minadd('inf',_,'inf').
minadd(_,'inf','inf').
minadd(A,B,Min):- Min is A+B.

maxadd('inf',_,'inf').
maxadd(_,'inf','inf').
maxadd('-inf',_,'-inf').
maxadd(_,'-inf','-inf').
maxadd(A,B,Max):- Max is A+B.

equal_wide_lists(CFGV1,CFGV2):-
    included(CFGV1,CFGV2),
    included(CFGV2,CFGV1).

included([],_).
included([b(N,Block,_,VarWideIntList1,NextBlocks)|CFGV],Ref):-
    included(CFGV,Ref),
    member(b(N,Block,_,VarWideIntList2,NextBlocks),Ref),
    equal_lists(VarWideIntList1,VarWideIntList2).


min_high('inf',_,'inf').
min_high('-inf',_,'-inf').
min_high(Y,[K|Ks],WY):-
    Y\=K, max(Y,K,Y), min_high(Y,Ks,WY).
min_high(Y,[K|_],K):-
    max(Y,K,K).
min_high(_,[],'inf').

max_low('-inf',_,'-inf',_).
max_low('inf',_,'inf',_).
max_low(X,[K|_],Best,Best):-
    X\=K, max(X,K,K).
max_low(X,[K|_],K,_):-
    X = K.
max_low(X,[K|Ks],WX,_):-
    max(X,K,X), max_low(X,Ks,WX,K).
max_low(_,[],Best,Best).

widening([],[]).
widening([b(N,Block,VarIntList,VarWideIntList,NextBlocks)|CFGV],[b(N,Block,VarIntList,VarWideIntList1,NextBlocks)|CFGV1]):-
    widening(CFGV,CFGV1),
    wAll(VarIntList,VarWideIntList,VarWideIntList1).

wAll([],_,[]).
wAll([vVal(Name,I)|VarIntList],VarWideIntList,[vVal(Name,WI1)|VarWideIntList1]):-
    member(vVal(Name,WI),VarWideIntList),!,
    wAll(VarIntList,VarWideIntList,VarWideIntList1),
    w(WI,I,WI1).
wAll([vVal(Name,I)|VarIntList],VarWideIntList,[vVal(Name,WI1)|VarWideIntList1]):-
    wAll(VarIntList,VarWideIntList,VarWideIntList1),
    w('mt',I,WI1).

