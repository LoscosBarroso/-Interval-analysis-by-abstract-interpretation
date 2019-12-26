:-module(_,_,[dcg]).

testskip(X):- X = "skip; skip;".

test0(X):- X = "varibale := true;".

test1(X):- X = "
if (s == 3) then 
   if (not true or false) xor (6==((23+var2)*3453452)) then 
   skip;
else
   variable := ( not true or false ) xor ( 6 == ( ( 23 + var2 ) * 3453452 ) ) ;
end 
else
   if (not true or false) xor (6==((23+var2)*3453452)) then 
   skip;
else
   variable := ( not true or false ) xor ( 6 == ( ( 23 + var2 ) * 3453452 ) ) ;
end 
end 
z := 3/(4);".

%AST = comp(if(boolExp(eq(numExp(numVar(s)),numExp(num(3)))),if(boolExp(xor(or(not(true),false),eq(numExp(num(6)),numExp(times(plus(num(23),numVar(var2)),num(3453452)))))),skip,assB(boolVar(variable),boolExp(xor(or(not(true),false),eq(numExp(num(6)),numExp(times(plus(num(23),numVar(var2)),num(3453452)))))))),if(boolExp(xor(or(not(true),false),eq(numExp(num(6)),numExp(times(plus(num(23),numVar(var2)),num(3453452)))))),skip,assB(boolVar(variable),boolExp(xor(or(not(true),false),eq(numExp(num(6)),numExp(times(plus(num(23),numVar(var2)),num(3453452))))))))),assN(numVar(z),numExp(div(num(3),num(4))))),
%X = [if,'(',s,==,3,')',then,if,'(',not,true,or,false,')',xor,'(',6,==,'(','(',23,+,var2,')',*,3453452,')',')',then,skip,;,else,variable,:=,'(',not,true,or,false,')',xor,'(',6,==,'(','(',23,+,var2,')',*,3453452,')',')',;,end,else,if,'(',not,true,or,false,')',xor,'(',6,==,'(','(',23,+,var2,')',*,3453452,')',')',then,skip,;,else,variable,:=,'(',not,true,or,false,')',xor,'(',6,==,'(','(',23,+,var2,')',*,3453452,')',')',;,end,end,z,:=,3,/,'(',4,')',;] 

test2(X):- X = "
if x <= 0 then
    y := 0;
else
    if x <= 1 then
      y := 1;
    else
        f1 := 0;
        f2 := 0;
        f1 := (x - 1);
        f2 := (x - (2*3123));
        y := f1 + f2;
    end
end
z := 3/(4);".

%AST = comp(if(boolExp(le(numExp(numVar(x)),numExp(num(0)))),assN(numVar(y),numExp(num(0))),if(boolExp(le(numExp(numVar(x)),numExp(num(1)))),assN(numVar(y),numExp(num(1))),comp(assN(numVar(f1),numExp(num(0))),comp(assN(numVar(f2),numExp(num(0))),comp(assN(numVar(f1),numExp(sub(numVar(x),num(1)))),comp(assN(numVar(f2),numExp(sub(numVar(x),times(num(2),num(3123))))),assN(numVar(y),numExp(plus(numVar(f1),numVar(f2)))))))))),assN(numVar(z),numExp(div(num(3),num(4))))),
%X = [if,x,<=,0,then,y,:=,0,;,else,if,x,<=,1,then,y,:=,1,;,else,f1,:=,0,;,f2,:=,0,;,f1,:=,'(',x,-,1,')',;,f2,:=,'(',x,-,'(',2,*,3123,')',')',;,y,:=,f1,+,f2,;,end,end,z,:=,3,/,'(',4,')',;]

test3(X):- X = "
if x <= 0 then
    y := 0;
else
    while x <= 1 do
      y := 1;
        f1 := 0;
        f2 := 0;
        f1 := (x - 1);
        f2 := (x - (2*3123));
        y := f1 + f2;
    end
end
z := 3/(4);".

%AST = comp(if(boolExp(le(numExp(numVar(x)),numExp(num(0)))),assN(numVar(y),numExp(num(0))),while(boolExp(le(numExp(numVar(x)),numExp(num(1)))),comp(assN(numVar(y),numExp(num(1))),comp(assN(numVar(f1),numExp(num(0))),comp(assN(numVar(f2),numExp(num(0))),comp(assN(numVar(f1),numExp(sub(numVar(x),num(1)))),comp(assN(numVar(f2),numExp(sub(numVar(x),times(num(2),num(3123))))),assN(numVar(y),numExp(plus(numVar(f1),numVar(f2))))))))))),assN(numVar(z),numExp(div(num(3),num(4)))))
%Z = [if,x,<=,0,then,y,:=,0,;,else,while,x,<=,1,do,y,:=,1,;,f1,:=,0,;,f2,:=,0,;,f1,:=,'(',x,-,1,')',;,f2,:=,'(',x,-,'(',2,*,3123,')',')',;,y,:=,f1,+,f2,;,end,end,z,:=,3,/,'(',4,')',;]

test4(X):- X = "
while true or (0<3) do
    y := 0;
    if x <= 1 then
      y := 1;
    else
        f1 := 0;
        f2 := 0;
        f1 := (x - 1);
        f2 := (x - (2*3123));
        y := f1 + f2;
    end
end
z := 3/(4);".
%AST = comp(while(boolExp(or(true,lt(numExp(num(0)),numExp(num(3))))),comp(assN(numVar(y),numExp(num(0))),if(boolExp(le(numExp(numVar(x)),numExp(num(1)))),assN(numVar(y),numExp(num(1))),comp(assN(numVar(f1),numExp(num(0))),comp(assN(numVar(f2),numExp(num(0))),comp(assN(numVar(f1),numExp(sub(numVar(x),num(1)))),comp(assN(numVar(f2),numExp(sub(numVar(x),times(num(2),num(3123))))),assN(numVar(y),numExp(plus(numVar(f1),numVar(f2))))))))))),assN(numVar(z),numExp(div(num(3),num(4)))))
%[while,true,or,'(',0,<,3,')',do,y,:=,0,;,if,x,<=,1,then,y,:=,1,;,else,f1,:=,0,;,f2,:=,0,;,f1,:=,'(',x,-,1,')',;,f2,:=,'(',x,-,'(',2,*,3123,')',')',;,y,:=,f1,+,f2,;,end,end,z,:=,3,/,'(',4,')',;]

test5(X):- X = "
y := 0;
while true or (0<3) do
    zas := 0;
    if y <= 1 then
      y := 1;
    else
        x := y/2;
        f1 := 0;
        f2 := 0;
        f1 := (x - 1);
        f2 := (x - (2*3123));
        y := f1 + f2;
    end
end
".

test6(X):- X = "
y := 0;
x := y+1;
y := x/2;
".

test7(X):- X = "

y := -0.345;
while true do
   z := -0.345;
   y := y+1;
   y := y*2;
end
".

[
    b(0,assN(numVar(y),numExp(neg(num(0.345)))),[],[],[1]),
    b(1,cond(boolExp(true)),[vVal(y,i(-0.345,421.68)),vVal(z,i(-0.345,-0.345))],[vVal(y,wi(-1,500)),vVal(z,wi(-1,0))],[2,5]),
    b(2,assN(numVar(z),numExp(neg(num(0.345)))),[vVal(y,i(-0.345,209.84)),vVal(z,i(-0.345,-0.345))],[vVal(y,wi(-1,500)),vVal(z,wi(-1,0))],[3]),
    b(3,assN(numVar(y),numExp(plus(numVar(y),num(1)))),[vVal(z,i(-0.345,-0.345)),vVal(y,i(-0.345,209.84))],[vVal(z,wi(-1,0)),vVal(y,wi(-1,500))],[4]),
    b(4,assN(numVar(y),numExp(times(numVar(y),num(2)))),[vVal(y,i(0.655,210.84)),vVal(z,i(-0.345,-0.345))],[vVal(y,wi(0,500)),vVal(z,wi(-1,0))],[1]),
    b(5,skip,[vVal(y,i(-0.345,209.84)),vVal(z,i(-0.345,-0.345))],[vVal(y,wi(-1,500)),vVal(z,wi(-1,0))],[])
]