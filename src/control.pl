:-module(_,[ast_to_cfg/5],[dcg]).
:-use_module(parser).
:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Generates a control flow graph from the abstract
%syntax tree of a program. Also pointers to the
%first block and ending blocks

ast_to_cfg(AST,CFG,NumBlocks,CFGHead,CFGEnds):-
    headendcfg(AST,0,NumBlocks,CFGHead,CFG,CFGEnds).


%headendcfg(Statement, FirstBlockIndex, NextIndexAfterBlock, FirstBlockOfTheCFG, AllCFGBlocks, EndCFGBlocks)
headendcfg(comp(AST1,AST2),0,Count,CFGHead,CFG,CFGEnds):-
    headcfg(AST1,0,Next,CFGHead,CFG1),
    endcfg(AST2,Next,Count,CFG2,CFGEnds),
    append(CFG1,CFG2,CFG).

headendcfg('skip',0,1,b(0,'skip',[]),[b(0,'skip',[])],[b(0,'skip',[])]).
headendcfg(assB(BoolVar,BoolExp),0,1,b(0,assB(BoolVar,BoolExp),[]),[b(0,assB(BoolVar,BoolExp),[])],[b(0,assB(BoolVar,BoolExp),[])]).
headendcfg(assN(NumVar,NumExp),0,1,b(0,assN(NumVar,NumExp),[]),[b(0,assN(NumVar,NumExp),[])],[b(0,assN(NumVar,NumExp),[])]).
headendcfg(if(BoolExp,AST1,AST2),0,Count,b(0,cond(BoolExp),[1,Else]),CFG,CFGEnds):-
    endcfg(AST1,   1, Else,CFG1,CFG1Ends),
    endcfg(AST2,Else,Count,CFG2,CFG2Ends),
    append([b(0,cond(BoolExp),[1,Else])|CFG1],CFG2,CFG),
    append(CFG1Ends,CFG2Ends,CFGEnds).
headendcfg(while(BoolExp,AST),0,Count,b(0,cond(BoolExp),[1,EndWhile]),[b(0,cond(BoolExp),[1,EndWhile]),b(EndWhile,'skip',[])|CFG],[b(EndWhile,'skip',[])]):-
    cfg(AST,1,EndWhile,0,CFG),
    Count is EndWhile+1.

%headcfg(Statement, FirstBlockIndex, NextIndexAfterBlock,  FirstBlockOfTheCFG, AllCFGBlocks)
headcfg(comp(AST1,AST2),0,Count,CFGHead,CFG):-
    headcfg(AST1,0,Next,CFGHead,CFG1),
    cfg(AST2,Next,Count,Count,CFG2),
    append(CFG1,CFG2,CFG).
headcfg('skip',0,1,b(0,'skip',[1]),[b(0,'skip',[1])]).
headcfg(assB(BoolVar,BoolExp),0,1,b(0,assB(BoolVar,BoolExp),[1]),[b(0,assB(BoolVar,BoolExp),[1])]).
headcfg(assN(NumVar,NumExp),0,1,b(0,assN(NumVar,NumExp),[1]),[b(0,assN(NumVar,NumExp),[1])]).
headcfg(if(BoolExp,AST1,AST2),0,Next,b(0,cond(BoolExp),[1,Else]),CFG):-
    cfg(AST1,   1,Else,Next,CFG1),
    cfg(AST2,Else,Next,Next,CFG2),
    append([b(0,cond(BoolExp),[1,Else])|CFG1],CFG2,CFG).
headcfg(while(BoolExp,AST),0,Next,b(0,cond(BoolExp),[1,Next]),[b(0,cond(BoolExp),[1,Next])|CFG]):-
    cfg(AST,1,Next,0,CFG).


%cfg(Statement, FirstBlockIndex, NextIndexAfterBlock, TargetIndexAfterBlock, AllCFGBlocks)
cfg(comp(AST1,AST2),Index,Next,Target,CFG):-
    cfg(AST1,Index,Next1,Next1,CFG1),
    cfg(AST2,Next1,Next ,Target, CFG2),
    append(CFG1,CFG2,CFG).
cfg('skip',N,N1,M,[b(N,'skip',[M])]):-
    N1 is N+1.
cfg(assB(BoolVar,BoolExp),N,N1,M,[b(N,assB(BoolVar,BoolExp),[M])]):-
    N1 is N+1.
cfg(assN(NumVar,NumExp),N,N1,M,[b(N,assN(NumVar,NumExp),[M])]):-
    N1 is N+1.
cfg(if(BoolExp,AST1,AST2),Index,Next,Target,CFG):-
    Then is Index+1,
    cfg(AST1,Then,Else,Target,CFG1),
    cfg(AST2,Else,Next,Target,CFG2),
    append([b(Index,cond(BoolExp),[Then,Else])|CFG1],CFG2,CFG).
cfg(while(BoolExp,AST),Index,Next,Target,[b(Index,cond(BoolExp),[Wfirst,Target])|CFG]):-
    Wfirst is Index+1,
    cfg(AST,Wfirst,Next,Index,CFG).


%headcfg(Statement, FirstBlockIndex, NextIndexAfterBlock, AllCFGBlocks, EndCFGBlocks)
endcfg(comp(AST1,AST2),Index,Count,CFG,CFGEnds):-
    cfg(AST1,Index,Next,Next,CFG1),
    endcfg(AST2,Next,Count,CFG2,CFGEnds),
    append(CFG1,CFG2,CFG).
endcfg('skip',N,N1,[b(N,'skip',[])],[b(N,'skip',[])]):-
    N1 is N+1.
endcfg(assB(BoolVar,BoolExp),N,N1,[b(N,assB(BoolVar,BoolExp),[])],[b(N,assB(BoolVar,BoolExp),[])]):-
    N1 is N+1.
endcfg(assN(NumVar,NumExp),N,N1,[b(N,assN(NumVar,NumExp),[])],[b(N,assN(NumVar,NumExp),[])]):-
    N1 is N+1.
endcfg(if(BoolExp,AST1,AST2),Index,Count,CFG,CFGEnds):-
    Then is Index+1,
    endcfg(AST1,Then, Else,CFG1,CFG1Ends),
    endcfg(AST2,Else,Count,CFG2,CFG2Ends),
    append([b(Index,cond(BoolExp),[Then,Else])|CFG1],CFG2,CFG),
    append(CFG1Ends,CFG2Ends,CFGEnds).
endcfg(while(BoolExp,AST),Index,Count,[b(Index,cond(BoolExp),[Wfirst,EndWhile]),b(EndWhile,'skip',[])|CFG],[b(EndWhile,'skip',[])]):-
    Wfirst is Index+1,
    cfg(AST,Wfirst,EndWhile,Index,CFG),
    Count is EndWhile+1.