expressions = [
    "let a = 1 in a + 1;",
    "let a = 1; b = 2 in a + b;",
    "let a = (1 - 3 * (2 - 1)); b = 2 in a + b;",
    "let a = (1 - 3 * (2 - 1)); b = 1 - (2 - 1) in a + b;",
    "if a == 1 then a else 2;",
    "if a == 1 && b == 2 then a + b else 22;",
    "if let a = 1 in a == 1 then a + 1 else 44;"
    ]

--- TODO functions Slide, Update, LetSlide, 



-- 2

-- 4

--     APP
-- +       3

-- Makeapp 
-- pushfun +
-- Makeapp
-- Makeapp 



-- 2

--         APP
--     APP     4
-- +       3

-- pushfun +
-- Makeapp
-- Makeapp


-- 2

--         APP
-- +               APP
--             APP      4
--         +       3   

-- Makeapp



--                         APP
--         APP                     2
-- +               APP
--             APP      4
--         +       3              


-- a b c + +






-- Definition von main


--             APP

--         APP      3

--     APP     2

-- +       1


-                 APP
-         APP             b
-     
    Plus         APP
-     
             APP      c

-     Plus       a


a + b + c


(Plus Plus a c, b)


b
c
a
Plus
Plus





-- + + 1 3 2

-- A = a + b + c

-- f a b c = a + b + c


--         + 
--     +       c
-- a       b   

-- a b + c + 



-- Üb(A3, Pos())
-- Üb(A2, Pos() + 1)
-- Üb(A1, Pos() + 2)

-- Pos(f) = ((a,1),(b,2),(c,3))

-- ÜbDef (a + b + c, Pos(f), 3) = 
    

--     Üb((+ (+ a b)) , c ), Pos(f))
--     Update 3
--     Slide 4
--     Unwind
--     Call
--     Return

--     Pushparam 3
--     ÜbKons((+ ,(+ a b))), Pos+1)
--     Makeapp
--     Update 3
--     Slide 4
--     Unwind
--     Call
--     Return

--     Pushparam 3
--     ÜbKons (+ a, b, Pos + 1)
--     Pushfun + (Pos + 2)
--     Makeapp
--     Makeapp
--     Update 3
--     Slide 4
--     Unwind
--     Call
--     Return

--     Pushparam 3
--     Pushparam 2 + 1
--     ÜbKons ((+ a), Pos + 2)
--     Makeapp
--     Pushfun +
--     Makeapp
--     Makeapp 
--     Update 3
--     Slide 4
--     Unwind
--     Call
--     Return

    
--     Pushparam 3
--     Pushparam 2
--     Pushparam 1
--     Pushfun +
--     Makeapp
--     Makeapp
--     Pushfun +
--     Makeapp
--     Makeapp 
--     Update 3
--     Slide 4
--     Unwind
--     Call
--     Return






G Machine
     Pushparam 2,
     Pushparam 4,
     Pushparam 3,
     Pushfun "+",
     Makeapp,
     Makeapp,
     Pushfun "+",
     Makeapp,
     Makeapp
     Update 3
     Slide 4
     Unwind
     Call
     Return


übkons(+ + a c b)

üb (b)
üb (c,Pos 2)
Makeapp
üb (a, Pos 2)
Pushfun plus
Makeapp
üb (plus)
Update 2
Slide 4
Unwind
Call
Return


Pushparam 2
übkons(+ a, Pos + 1)
Makeapp
Update 2
Slide 4
Unwind
Call
Return

Pushparam 2
übkons(a),Pos + 1
Pushfun + , Pos + 1 + Pos + 1
Makeapp
Makeapp
Update 2
Slide 4
Unwind
Call
Return

Pushparam 2
pushparam 2
pushfun +
makeapp
makeapp
Update 2
Slide 4
Unwind
Call
Return

übkons(+, + a c b)
Update 2
Slide 4
Unwind
Call
Return

übkons( +, a c b) pos + 1)
Pushfun +
Makeapp
Update 2
Slide 4
Unwind
Call
Return

übkons(a, c b), pos + 2)
Pushfun +
MakeApp
Pushfun +
Makeapp
Update 2
Slide 4
Unwind
Call
Return

Pushparam 2
Pushparam 4
Pushparam 3
PushFun+
PushFun+
Makeapp
Makeapp
Makeapp
Makeapp
Update 2
Slide 4
Unwind
Call
Return




--                 APP

--         APP             b

--     +       APP

--         APP      c

--     +       a









-- Üb(b, 2) = pushparam 2
-- Üb(c, 3 + 1) = pushparam 4
-- Üb(a, 1 + 2) = pushparam 3





üb (b)
üb (c,Pos 2)
üb (a, Pos 2)
Pushfun plus
Makeapp
üb (plus)







Üb(+bc)
Üb(+a)
Makeapp
Update 2
Slide 4
Unwind
Call
Return



Üb(c)
Üb(+b)
MakeApp
Üb(a)
Üb(+)
Makeapp
Makeapp
Update 2
Slide 4
Unwind
Call
Return



Üb(c)
Üb(b)
Üb(+)
MakeApp
MakeApp
Üb(a)
Üb(+)
Makeapp
Makeapp
Update 2
Slide 4
Unwind
Call
Return




tr(b)
tr(c)
tr(+(+ a))
makeapp
makeapp
Update 2
Slide 4
Unwind
Call
Return

+ 6 )




tr(6)
tr(+)
makeapp
makeapp
Update 2
Slide 4
Unwind
Call
Return


(((+ ((+ a) b) c)

pushfun a
makeapp
Update 2
Slide 4
Unwind
Call
Return



((+ 6  )(( * 4) 3))

+ + 6 4 3
+ 6 * 4 3





üb(* 43)
üb(+ 6)
Makeapp

üb(* 43)
üb 6
üb +
Makeapp
Makeapp

üb 3
üb((* 4)
Makeapp
üb 6
üb +
Makeapp
Makeapp

üb 3
üb(4)
üb *
Makeapp
Makeapp
üb 6
üb +
Makeapp
Makeapp


App Tree -> Instruction -> App Trees





(+ + a b)

(((Plus ((Plus a) c) b)


(App (App Plus (App (App Plus a) b)) c)



((((+ +) a) b) c)
+ (+ a b) c


üb c
üb (+ (+ a b))
makeapp

üb c
üb ((+ a) b))
pushfun +
makeapp
makeapp


üb c
üb b
üb (+ a)
makeapp
pushfun +
makeapp
makeapp

üb 3
üb 4
üb 6
üb (+)
makeapp
makeapp
pushfun +
makeapp
makeapp







üb c
üb b
üb (((+ +) a))
makeapp
makeapp


üb c
üb b
üb a
üb (((+ +)))
makeapp
makeapp
makeapp



(+ 1) ((+ 2) 3)


üb ((+ 2) 3)
üb (+ 1)
makeapp


üb ((+ 2) 3)
pushval 1
pushfun +
makeapp
makeapp


pushval 3
üb ((+ 2))
makeapp
pushval 1
pushfun +
makeapp
makeapp


pushval 3
pushval 2
pushfun + 
makeapp
makeapp
pushval 1
pushfun +
makeapp
makeapp




(+ ((+ 1) 2)) 3


pushval 3
üb (+ ((+ 1) 2))
makeapp

pushval 3
üb ((+ 1) 2)
pushfun + 
makeapp
makeapp

pushval 3
pushval 2
üb ((+ 1))
makeapp
pushfun + 
makeapp
makeapp

pushval 3
pushval 2
pushval 1
pushfun +
makeapp
makeapp
pushfun + 
makeapp
makeapp



üb 3
üb (((+) +) 1) 2)
makeapp






+ 4) +) 5) 6)

pushval 6
üb + 4) +) 5)
makeapp


pushval 6
pushval 5
üb + 4) +))
makeapp
makeapp