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


übkons(+ a, b)
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