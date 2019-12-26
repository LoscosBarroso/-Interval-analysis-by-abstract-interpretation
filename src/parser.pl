:-module(_,[ast/2],[dcg]).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ast(TokenList,AST):-
    phrase(program(AST),TokenList).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Abstract Syntaxt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Numeric Expressions
nExp(X):- number(X).
nExp(X):- nVar(X).


%Boolean Expressions
bExp(true).
bExp(false).
bExp(X):- bVar(X).

%Reserved Words
reservedW('true').
reservedW('false').
reservedW('and').
reservedW('or').
reservedW('not').
reservedW('xor').
reservedW('while').
reservedW('do').
reservedW('end').
reservedW('if').
reservedW('then').
reservedW('else').
reservedW('skip').
reservedS(';').
reservedS('+').
reservedS('-').
reservedS('*').
reservedS('/').
reservedS(')').
reservedS('(').
reservedS('==').
reservedS('!=').
reservedS('>=').
reservedS('<=').
reservedS('>').
reservedS('<').
reservedS(':=').
reserved(X) :- reservedW(X),!.
reserved(X) :- reservedS(X).

%Variable names
nVar(X):- reserved(X),!,fail.
nVar(X):- atom(X).
bVar(X):- reserved(X),!,fail.
bVar(X):- atom(X).

boolval(true). boolval(false).
boolval(X):- bVar(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG's for AST construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%A program is a chain of statements
program(AST) --> statements(AST).
statements(comp(StmAST,RestAST)) --> statement(StmAST), statements(RestAST).
statements(AST) --> statement(AST).
statement(_) -->  \+ [_], !, {fail}.

%we have the following statements
statement(skip) --> ['skip'], [';'],!.
statement(assN(numVar(Name),numExp(NumAST)))-->[Name],{nVar(Name)}, [':='], sum(NumAST), [';'].
statement(assB(boolVar(Name),boolExp(BoolAST)))--> [Name],{bVar(Name)},!, [':='], bool(BoolAST), [';'].
statement(if(boolExp(BoolAST),StmASTtt,StmASTff))--> ['if'],!, bool(BoolAST), ['then'], statements(StmASTtt), ['else'], statements(StmASTff) , ['end'].
statement(while(boolExp(BoolAST),StmAST))--> ['while'],!, bool(BoolAST), ['do'], statements(StmAST), ['end'].

%Numeric Expressions
sum(TotalAST) --> mul(MulAST), ['+'], resum(MulAST,'+',TotalAST).
sum(TotalAST) --> mul(MulAST), ['-'], resum(MulAST,'-',TotalAST).
sum(AST) --> mul(AST).
resum(PrevAST,'+',TotalAST) --> mul(MulAST), ['+'], resum(plus(PrevAST,MulAST),'+',TotalAST).
resum(PrevAST,'-',TotalAST) --> mul(MulAST), ['+'], resum(sub(PrevAST,MulAST),'+',TotalAST).
resum(PrevAST,'+',TotalAST) --> mul(MulAST), ['-'], resum(plus(PrevAST,MulAST),'-',TotalAST).
resum(PrevAST,'-',TotalAST) --> mul(MulAST), ['-'], resum(sub(PrevAST,MulAST),'-',TotalAST).
resum(PrevAST,'+',plus(PrevAST,MulAST)) --> mul(MulAST).
resum(PrevAST,'-',sub(PrevAST,MulAST)) --> mul(MulAST).


mul(TotalAST) --> ter(TerAST), ['*'], remul(TerAST,'*',TotalAST).
mul(TotalAST) --> ter(TerAST), ['/'], remul(TerAST,'/',TotalAST).
mul(AST) --> ter(AST).
remul(PrevAST,'*',TotalAST) --> ter(TerAST), ['*'], remul(times(PrevAST,TerAST),'*',TotalAST).
remul(PrevAST,'/',TotalAST) --> ter(TerAST), ['*'], remul(div(PrevAST,TerAST),'*',TotalAST).
remul(PrevAST,'*',TotalAST) --> ter(TerAST), ['/'], remul(times(PrevAST,TerAST),'/',TotalAST).
remul(PrevAST,'/',TotalAST) --> ter(TerAST), ['/'], remul(div(PrevAST,TerAST),'/',TotalAST).
remul(PrevAST,'*',times(PrevAST,TerAST)) --> ter(TerAST).
remul(PrevAST,'/',div(PrevAST,TerAST)) --> ter(TerAST).


ter(neg(AST)) --> ['-'],!, ter(AST).
ter(AST) --> ['('],!, sum(AST), [')'].
ter(num(N)) --> [N],{number(N)}.
ter(numVar(Name)) --> [Name],{nVar(Name)}.

%Boolean Expressions
bool(TotalAST) --> bool2(TwoAST), ['or'], rebool(TwoAST,'or',TotalAST).
bool(AST) --> bool2(AST).
rebool(PrevAST,'or',TotalAST) --> bool2(TwoAST), ['or'], rebool(or(PrevAST,TwoAST),'or',TotalAST).
rebool(PrevAST,'or',or(PrevAST,TwoAST)) --> bool2(TwoAST).


bool2(TotalAST) --> bool3(TwoAST), ['xor'], rebool2(TwoAST,'xor',TotalAST).
bool2(AST) --> bool3(AST).
rebool2(PrevAST,'xor',TotalAST) --> bool3(TwoAST), ['xor'], rebool2(xor(PrevAST,TwoAST),'xor',TotalAST).
rebool2(PrevAST,'xor',xor(PrevAST,TwoAST)) --> bool3(TwoAST).


bool3(TotalAST) --> bool4(TwoAST), ['and'], rebool3(TwoAST,'and',TotalAST).
bool3(AST) --> bool4(AST).
rebool3(PrevAST,'and',TotalAST) --> bool4(TwoAST), ['and'], rebool3(and(PrevAST,TwoAST),'and',TotalAST).
rebool3(PrevAST,'and',or(PrevAST,TwoAST)) --> bool4(TwoAST).


bool4(eq(ASTL,ASTR)) --> bool5(ASTL), ['=='], bool5(ASTR).
bool4(ne(ASTL,ASTR)) --> bool5(ASTL), ['!='], bool5(ASTR).
bool4(AST) --> bool5(AST).
bool4(eq(numExp(NumASTL),numExp(NumASTR))) --> ter(NumASTL), ['=='],!, ter(NumASTR).
bool4(ne(numExp(NumASTL),numExp(NumASTR))) --> ter(NumASTL), ['!='],!, ter(NumASTR).

bool5(AST) --> bool6(AST).
bool5(lt(numExp(NumASTL),numExp(NumASTR))) --> ter(NumASTL), ['<'],!, ter(NumASTR).
bool5(le(numExp(NumASTL),numExp(NumASTR))) --> ter(NumASTL), ['<='],!, ter(NumASTR).
bool5(gt(numExp(NumASTL),numExp(NumASTR))) --> ter(NumASTL), ['>'],!, ter(NumASTR).
bool5(ge(numExp(NumASTL),numExp(NumASTR))) --> ter(NumASTL), ['>='],!, ter(NumASTR).

bool6(not(AST)) --> ['not'],!, bool6(AST).
bool6(AST) --> ['('],!, bool(AST), [')'].
bool6('true') --> ['true'],!.
bool6('false') --> ['false'],!.
bool6(boolVar(Name)) --> [Name],{bVar(Name)}.




% program("tokenized source code in a Prolog list",_,X).
% can be invoked directly.  This has the small advantage of bypassing
% the single-character identifier limitation of the lexer.
% On return, X contains the desired intermediate code.

%phrase(program(AST),['skip',';','skip',';']).

%phrase(num1(AST),[23,'+',var2,'+',var3,'-',4,'-',5,'+','var6']).

%phrase(program(AST),['variable',':=','(','not','true','or','false',')','xor','(',6,'==', '(','(',23,'+',var2,')','*',3453452,')',')',';']).

%phrase(program(AST),['if','(','not','true','or','false',')','xor','(',6,'==', '(','(',23,'+',var2,')','*',3453452,')',')','then','skip',';','else','variable',':=','(','not','true','or','false',')','xor','(',6,'==', '(','(',23,'+',var2,')','*',3453452,')',')',';','end']).

%phrase(program(AST),['while','true','do','if','(','not','true','or','false',')','xor','(',6,'==', '(','(',23,'+',var2,')','*',3453452,')',')','then','skip',';','else','variable',':=','(','not','true','or','false',')','xor','(',6,'==', '(','(',23,'+',var2,')','*',3453452,')',')',';','skip',';','end','skip',';','end']).
