{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CodeGeneration where
import Data.Maybe
import Parser
import Helpers
import Lexer
import DataStructures
import Instructions

-------------------------- Translation--------------------------
translateProg xs = foldr (\ x -> (++) (translateDef x [])) [] xs

translateDef def list =
    case def of
        VarDef name expr -> translateVar name expr list
        FuncDef name args expr -> translateFunc name args expr list ([], createLocalEnv args 1)
        Deps -> list

translateVar name expr list = translateExpr expr ([],[]) ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list 

translateFunc name args expr list globalEnv = translateExpr expr globalEnv ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return] ++ list

translateExpr expr (letEnv, locEnv) = 
    case expr of
        VarX a -> if isInEnv a letEnv then [Pushparam (getPos a letEnv)] else if isInEnv a locEnv then [Pushparam (getPos a locEnv)] else [Pushfun a]
        IntX a -> [Pushval (IntX a)]
        BoolX a -> [Pushfun (show a)]
        e@(PlusX expr1 expr2) -> translatePlus e (letEnv, locEnv)
        e@(MinusX expr expr2) -> translateMinus e (letEnv, locEnv)
        e@(MultX expr expr2) -> translateMult e (letEnv, locEnv)
        e@(DivX expr1 expr2) -> translateDiv e (letEnv, locEnv)
        e@(OrX expr expr2) -> translateOr e (letEnv, locEnv) 
        e@(AndX expr expr2) -> translateAnd e (letEnv, locEnv)
        e@(SmallerX expr expr2) -> translateSmaller e (letEnv, locEnv)
        e@(EqualsX expr expr2) -> translateEquals e (letEnv, locEnv)
        (NotX expr) -> translateExpr expr (letEnv, locEnv) ++ push "NotX"
        (NegX expr) -> translateExpr expr (letEnv, locEnv) ++ push "Negate"
        e@(IfX expr1 expr2 expr3) -> translateIf e (letEnv, locEnv)
        (LetX locdefs expr) -> translateLet locdefs expr (createLetEnv locdefs ([],locEnv) 0)

translateLet locdefs expr env@(letEnv, localEnv) = translateLetExpr locdefs env ++ translateExpr expr env ++ [SlideLet (length letEnv)] 

translatePlus (PlusX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "+"
translatePlus (PlusX expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (PlusX expr1 EmptyExpr) (incrementPos globalEnv)
        
translateMinus (MinusX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push"-"
translateMinus (MinusX expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (MinusX expr1 EmptyExpr) (incrementPos globalEnv)

translateMult (MultX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "*"
translateMult (MultX expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (MultX expr1 EmptyExpr) (incrementPos globalEnv)

translateDiv (DivX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "/"
translateDiv (DivX expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (DivX expr1 EmptyExpr) (incrementPos globalEnv)

translateOr (OrX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "|"
translateOrX (OrX expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (OrX expr EmptyExpr) (incrementPos globalEnv)

translateAnd (AndX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "&"
translateAnd (AndX expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (AndX expr EmptyExpr) (incrementPos globalEnv)

translateSmaller (SmallerX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "<"
translateSmaller (SmallerX expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (SmallerX expr EmptyExpr) (incrementPos globalEnv)

translateEquals (EqualsX expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "=="
translateEquals (EqualsX expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (EqualsX expr EmptyExpr) (incrementPos globalEnv)

translateIf (IfX expr1 EmptyExpr EmptyExpr) globalEnv = translateExpr expr1 globalEnv ++ push "if"
translateIf (IfX expr1 expr2 EmptyExpr) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (IfX expr1 EmptyExpr EmptyExpr) (incrementPos globalEnv)
translateIf (IfX expr1 expr2 expr3) globalEnv = makeapp $ translateExpr expr3 globalEnv ++ translateExpr (IfX expr1 expr2 EmptyExpr) (incrementPos globalEnv)

translateLetExpr ((LocDef name expr):xs) (letEnv, localEnv) = translateExpr expr (letEnv, localEnv) ++ [Alloc, Makeapp] ++ translateLetExpr xs (incrementPos (letEnv, localEnv))
translateLetExpr [] _ = []

--------------------Environments--------------------
createLocalEnv (x:xs) counter = (x,counter) : createLocalEnv xs (counter+1)
createLocalEnv [] _ = []

createLetEnv ((LocDef name expr):xs) (letEnv, localEnv) counter = createLetEnv xs ([(name,counter+1)] ++ letEnv, localEnv) (counter+1)
createLetEnv [] (letEnv, localEnv) counter =  (letEnv, localEnv)

--------------------Helpers--------------------
makeapp a = a ++ [Makeapp]
push op = [Pushfun op, Makeapp]

isInEnv a ((x,y):xs) = (a == x) || isInEnv a xs
isInEnv a [] = False

getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 0

incrementPos ([], a) = ([], increment a)
incrementPos (a, b) = (increment a, b)

increment = map(\(x,y) -> (x,y+1))


--------------------Test Cases--------------------
testProg2 = [FuncDef "a" ["a", "b"] (PlusX (VarX "a") (VarX "b"))]
testProg8 = [VarDef "a" (IfX (BoolX True) (IntX 1) (IntX 2))] ++ testProg2
testProg = [VarDef "a" (BoolX True), VarDef "b" (IntX 2)]
testProg3 = [VarDef "a" (IntX 1) ,VarDef "b" (PlusX (VarX "a") (IntX 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (AndX (BoolX True) (BoolX False))]
testProg5 = [VarDef "x" (LetX [LocDef "a" (IntX 8), LocDef "b" (IntX 13)] (PlusX (VarX "a") (VarX "b")))]