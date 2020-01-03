:-module(_,_,[]).
   
:- use_module(tests).
:- use_module(lexer).
:- use_module(parser).
:- use_module(control).
:- use_module(intervals).
:- use_module(library(stream_utils)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(pathnames)).
:- use_module(engine(runtime_control)).


%The lexer and parser modules were edited from the versions at
%http://faculty.cooper.edu/smyth/cs225/ch7/prolog.htm


%To run this module just load it into a ciao interpreter and type:

% run(TokenList,AST,CFG,CFGSize,CFGHead,CFGEnds,_,Fixpoint,NumIterations,_,FixpointWithWidening,NumIterationsWithWidening).

run(Z,AST,CFG,CFGSize,CFGHead,CFGEnds,KCFG,Fixpoint,N,WKCFG,WFixpoint,WN):-
    test7(Input), %go to tests.pl and choose which example code to run (you can also write your own).
    format("~s",[Input]), %prints the while program to analyze
    lex(Input,Z), %lexical analysis
    ast(Z,AST), %AST construction
    ast_to_cfg(AST,CFG,CFGSize,CFGHead,CFGEnds), %CFG construction
    kleenitialize(CFG,KCFG), %Abstract intrpretation without widening
    kleeniteration(KCFG,Fixpoint,0,N,20),
    kleenitializew(CFG,WKCFG), %Abstrct interpretation with widening
    kleeniterationw(WKCFG,WFixpoint,0,WN).

