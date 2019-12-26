:-module(lexer,lex/2,[dcg]).

:- use_module(library(lists)).
/*Adapted from lexer.pl  http://faculty.cooper.edu/smyth/cs225/prolog/lexer.txt */
% The rules in this file implement the lexical analysis phase of compilation,
% by converting the source file character sequence into a Prolog list
% consisting of tokens for keywords, operators and identifiers.


% Note that a more compact syntax could have been used, but I decided to make the clauses as explicit as possible.
lex(InputString,OutputList):-
    phrase(tokens(OutputList),InputString).
% Create the token for statement separation.
tokens(Z) --> ";",!, tokens(Y), {Z = [';' | Y]}.

% Return a unique token for each control structure keyword in the form of
% a Prolog atom whose name literally matches the keyword.
tokens(Z) --> [C],"while", {sep(C)},!, tokens(Y), {Z = [while | Y]}.
tokens(Z) --> [C],"do", {sep(C)},!, tokens(Y), {Z = [do | Y]}.
tokens(Z) --> [C],"end", {sep(C)},!, tokens(Y), {Z = [end | Y]}.
tokens(Z) --> [C],"if", {sep(C)},!, tokens(Y), {Z = [if | Y]}.
tokens(Z) --> [C],"then", {sep(C)},!, tokens(Y), {Z = [then | Y]}.
tokens(Z) --> [C],"else", {sep(C)},!, tokens(Y), {Z = [else | Y]}.
tokens(Z) --> [C],"skip", {sep(C)},!, tokens(Y), {Z = [skip | Y]}.

% Comparison operators.
tokens(Z) --> "==",!, tokens(Y), {Z = [== | Y]}.
tokens(Z) --> "!=",!, tokens(Y), {Z = ['!=' | Y]}.
tokens(Z) --> ">=",!, tokens(Y), {Z = [>= | Y]}.
tokens(Z) --> "<=",!, tokens(Y), {Z = [<= | Y]}.
tokens(Z) --> ">",!, tokens(Y), {Z = [> | Y]}.
tokens(Z) --> "<",!, tokens(Y), {Z = [< | Y]}.

% Assignment operator.
tokens(Z) --> ":=",!, tokens(Y), {Z = [:= | Y]}.  

% Boolean constants and operators.
tokens(Z) --> [C],"true", {sep(C)},!, tokens(Y), {Z = [true | Y]}.  
tokens(Z) --> [C],"false", {sep(C)},!, tokens(Y), {Z = [false | Y]}.
tokens(Z) --> [C],"and", {sep(C)},!, tokens(Y), {Z = [and | Y]}.  
tokens(Z) --> [C],"or", {sep(C)},!, tokens(Y), {Z = [or | Y]}.
tokens(Z) --> [C],"not", {sep(C)},!, tokens(Y), {Z = [not | Y]}.  
tokens(Z) --> [C],"xor", {sep(C)},!, tokens(Y), {Z = [xor | Y]}.

% Numeral constants and operators.
tokens(Z) --> "+",!, tokens(Y), {Z = ['+' | Y]}.  
tokens(Z) --> "-",!, tokens(Y), {Z = ['-' | Y]}.
tokens(Z) --> "*",!, tokens(Y), {Z = ['*' | Y]}.  
tokens(Z) --> "/",!, tokens(Y), {Z = ['/' | Y]}.

% Parenthesis operators
tokens(Z) --> "(",!, tokens(Y), {Z = ['(' | Y]}.
tokens(Z) --> ")",!, tokens(Y), {Z = [')' | Y]}.

% Strip spaces, tabs and newlines.
tokens(Z) --> " ",!, tokens(Y), {Z = Y}.
tokens(Z) --> "\t",!, tokens(Y), {Z = Y}.
tokens(Z) --> "\n",!, tokens(Y), {Z = Y}.

% Anything not mentioned above gets its own token,
% including single-character identifiers.
tokens(Z) --> identifier(X),!, tokens(Y), {build(X,Y,Z)}.
tokens([]) --> \+ [_], !. %To match EOS

%whatever not parsed previously is considered single character tokens, this should not happen though.
tokens(Z) --> [C], tokens(Y), {name(X, [C]), Z = [X | Y]}.
tokens(Z) --> [], {Z = []}.

build([A|As],B,C) :-
    build(As,B,C1),
    C = [A | C1].
build([],B,B).

%Implementing the DCGs for identifiers and numbers:
identifier(Z) --> [C],id2([C],Z).
id2(X,[Xa]) --> " ",!, {name(Xa, X)}.
id2(X,[Xa]) --> "\t",!, {name(Xa, X)}.
id2(X,[Xa]) --> "\n",!, {name(Xa, X)}.
id2(X,[Xa, ';']) --> ";",!, {name(Xa, X)}.
id2(X,[Xa, '+']) --> "+",!, {name(Xa, X)}.
id2(X,[Xa, '-']) --> "-",!, {name(Xa, X)}.
id2(X,[Xa, '*']) --> "*",!, {name(Xa, X)}.
id2(X,[Xa, '/']) --> "/",!, {name(Xa, X)}.
id2(X,[Xa, ')']) --> ")",!, {name(Xa, X)}.
id2(X,[Xa, '(']) --> "(",!, {name(Xa, X)}.
id2(X,[Xa, '==']) --> "==",!, {name(Xa, X)}.
id2(X,[Xa, '!=']) --> "!=",!, {name(Xa, X)}.
id2(X,[Xa, '>=']) --> ">=",!, {name(Xa, X)}.
id2(X,[Xa, '<=']) --> "<=",!, {name(Xa, X)}.
id2(X,[Xa, '>']) --> ">",!, {name(Xa, X)}.
id2(X,[Xa, '<']) --> "<",!, {name(Xa, X)}.
id2(X,[Xa, ':=']) --> ":=",!, {name(Xa, X)}.
id2(X,Z) --> [C], {append(X,[C],X1)}, id2(X1,Z).
id2(X,X) --> \+ [_], !. %To match EOS

%Auxiliary definitions
sep(" ").
sep("\t").
sep("\n").

% If you now invoke the goal
% 
%        phrase(tokens(Z),"while a == b do other ; other endwhile")
% 
% then this goal will succeed and Z will be instantiated to 
%    
%       [while,a,==,b,do,other,;,other,endwhile].

/*
 phrase(tokens(Z),"  if x <= 0 then
    y := 0
  else
    if x <= 1 then
      y := 1
    else
      begin
        var f1 := 0;
        var f2 := 0;
        f1 := fib(x - 1);
        f2 := fib(x - (2*3123));
        y := f1 + f2;
      end
    end
  end
end

z := fib(4);").
                                                   
Z = [if,x,<=,0,then,y,:=,0,else,if,x,<=,1,then,y,:=,1,else,begin,var,f1,:=,0,;,var,f2,:=,0,;,f1,:=,fib,'(',x,-,1,')',;,f2,:=,fib,'(',x,-,'(',2,*,3123,')',')',;,y,:=,f1,+,f2,;,end,end,end,end,z,:=,fib,'(',4,')',;] 
*/