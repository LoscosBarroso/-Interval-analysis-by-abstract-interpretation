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
z := 3/(4);
skip;".

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
z := 3/(4);
skip;".


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

test8(X):- X = "

y := 0.345;
while true do
   z := 0.345;
   y := y-1;
   y := y*2;
end
".
