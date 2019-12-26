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

/*main([File]):-
    working_directory(CWD,CWD),
    directory_files(CWD,Files),
    member(File,Files),
    atom_concat(_,'.w',File),
    path_concat(CWD,File,F),
    file_to_string(F,Code),
    lex(Code,TokenList),
    ast(TokenList,AST),
    ast_to_cfg(AST,CFG,CFGHead,CFGEnds),
    interval_analysis(CFG,ICFG),
    constant_propagatopn(ICFG,CCFG).
*/


test(Z,AST,CFG,CFGSize,CFGHead,CFGEnds,KCFG,Fixpoint,N):-
    test7(Input),
    format("~s",[Input]),
    lex(Input,Z),
    ast(Z,AST),
    ast_to_cfg(AST,CFG,CFGSize,CFGHead,CFGEnds),
    kleenitialize(CFG,KCFG),
    kleeniteration(KCFG,Fixpoint,0,N).
                                                   
