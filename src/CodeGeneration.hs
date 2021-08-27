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
        Var a -> if isInEnv a locEnv then [Pushparam (getPos a locEnv)] else [Pushfun a]
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushfun (show a)]
        e@(Plus expr1 expr2) -> translatePlus e (letEnv, locEnv)
        e@(Minus expr expr2) -> translateMinus e (letEnv, locEnv)
        e@(Mult expr expr2) -> translateMult e (letEnv, locEnv)
        e@(Div expr1 expr2) -> translateDiv e (letEnv, locEnv)
        e@(Or expr expr2) -> translateOr e (letEnv, locEnv) 
        e@(And expr expr2) -> translateAnd e (letEnv, locEnv)
        e@(Smaller expr expr2) -> translateSmaller e (letEnv, locEnv)
        e@(Equals expr expr2) -> translateEquals e (letEnv, locEnv)
        (Not expr) -> translateExpr expr (letEnv, locEnv) ++ push "Not"
        (Neg expr) -> translateExpr expr (letEnv, locEnv) ++ push "Negate"
        e@(If expr1 expr2 expr3) -> translateIf e (letEnv, locEnv)
        (Let locdefs expr) -> translateLet locdefs expr (createLetEnv locdefs ([],locEnv) 0)

translateLet locdefs expr env@(letEnv, localEnv) = translateLetExpr locdefs env ++ translateExpr expr env ++ [SlideLet (length letEnv)] 

translatePlus (Plus expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "+"
translatePlus (Plus expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (Plus expr1 EmptyExpr) (incrementPos globalEnv)
        
translateMinus (Minus expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push"-"
translateMinus (Minus expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (Minus expr1 EmptyExpr) (incrementPos globalEnv)

translateMult (Mult expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "*"
translateMult (Mult expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (Mult expr1 EmptyExpr) (incrementPos globalEnv)

translateDiv (Div expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "/"
translateDiv (Div expr1 expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (Div expr1 EmptyExpr) (incrementPos globalEnv)

translateOr (Or expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "|"
translateOr (Or expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (Or expr EmptyExpr) (incrementPos globalEnv)

translateAnd (And expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "&"
translateAnd (And expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (And expr EmptyExpr) (incrementPos globalEnv)

translateSmaller (Smaller expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "<"
translateSmaller (Smaller expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (Smaller expr EmptyExpr) (incrementPos globalEnv)

translateEquals (Equals expr EmptyExpr) globalEnv = translateExpr expr globalEnv ++ push "=="
translateEquals (Equals expr expr2) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (Equals expr EmptyExpr) (incrementPos globalEnv)

translateIf (If expr1 EmptyExpr EmptyExpr) globalEnv = translateExpr expr1 globalEnv ++ push "if"
translateIf (If expr1 expr2 EmptyExpr) globalEnv = makeapp $ translateExpr expr2 globalEnv ++ translateExpr (If expr1 EmptyExpr EmptyExpr) (incrementPos globalEnv)
translateIf (If expr1 expr2 expr3) globalEnv = makeapp $ translateExpr expr3 globalEnv ++ translateExpr (If expr1 expr2 EmptyExpr) (incrementPos globalEnv)

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
testProg2 = [FuncDef "a" ["a", "b"] (Plus (Var "a") (Var "b"))]
testProg8 = [VarDef "a" (If (Bool True) (Int 1) (Int 2))] ++ testProg2
testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]
testProg5 = [VarDef "x" (Let [LocDef "a" (Int 8), LocDef "b" (Int 13)] (Plus (Var "a") (Var "b")))]