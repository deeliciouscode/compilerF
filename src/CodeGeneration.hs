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

translateFunc name args expr list env = translateExpr expr env ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return] ++ list

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
        (NotX expr) -> translateExpr expr (letEnv, locEnv) ++ push "Not"
        (NegX expr) -> translateExpr expr (letEnv, locEnv) ++ push "Negate"
        e@(IfX expr1 expr2 expr3) -> translateIf e (letEnv, locEnv)
        (LetX locdefs expr) -> translateLet locdefs expr (createLetEnv locdefs ([],locEnv) 1) []

translateLet [] EmptyExpr env@(letEnv, localEnv) instructions = instructions ++ [SlideLet (length letEnv)]
translateLet [] expr env@(letEnv, localEnv) instructions = instructions ++ translateExpr expr env
translateLet locdefs expr env@(letEnv, localEnv) [] = translateLetExpr locdefs env [] expr

translateLetExpr [] env@(letEnv, localEnv) instructions globalExpr = translateLet [] EmptyExpr env (instructions ++ translateExpr globalExpr env) 
translateLetExpr ((LocDef name expr):xs) env@(letEnv, localEnv) instructions globalExpr =  translateLetExpr xs (incrementPos env) (translateExpr expr env ++ [Alloc, Makeapp] ++ instructions) globalExpr

translatePlus (PlusX expr EmptyExpr) env = translateExpr expr env ++ push "+"
translatePlus (PlusX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (PlusX expr1 EmptyExpr) (incrementPos env)
        
translateMinus (MinusX expr EmptyExpr) env = translateExpr expr env ++ push"-"
translateMinus (MinusX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (MinusX expr1 EmptyExpr) (incrementPos env)

translateMult (MultX expr EmptyExpr) env = translateExpr expr env ++ push "*"
translateMult (MultX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (MultX expr1 EmptyExpr) (incrementPos env)

translateDiv (DivX expr EmptyExpr) env = translateExpr expr env ++ push "/"
translateDiv (DivX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (DivX expr1 EmptyExpr) (incrementPos env)

translateOr (OrX expr EmptyExpr) env = translateExpr expr env ++ push "|"
translateOr (OrX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (OrX expr EmptyExpr) (incrementPos env)

translateAnd (AndX expr EmptyExpr) env = translateExpr expr env ++ push "&"
translateAnd (AndX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (AndX expr EmptyExpr) (incrementPos env)

translateSmaller (SmallerX expr EmptyExpr) env = translateExpr expr env ++ push "<"
translateSmaller (SmallerX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (SmallerX expr EmptyExpr) (incrementPos env)

translateEquals (EqualsX expr EmptyExpr) env = translateExpr expr env ++ push "=="
translateEquals (EqualsX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (EqualsX expr EmptyExpr) (incrementPos env)

translateIf (IfX expr1 EmptyExpr EmptyExpr) env = translateExpr expr1 env ++ push "if"
translateIf (IfX expr1 expr2 EmptyExpr) env = makeapp $ translateExpr expr2 env ++ translateExpr (IfX expr1 EmptyExpr EmptyExpr) (incrementPos env)
translateIf (IfX expr1 expr2 expr3) env = makeapp $ translateExpr expr3 env ++ translateExpr (IfX expr1 expr2 EmptyExpr) (incrementPos env)

--------------------Environments--------------------
createLocalEnv (x:xs) counter = (x,counter+1) : createLocalEnv xs (counter+1)
createLocalEnv [] _ = []

createLetEnv ((LocDef name expr):xs) (letEnv, localEnv) counter = createLetEnv xs ([(name,counter)] ++ letEnv, localEnv) (counter+1)
createLetEnv [] (letEnv, localEnv) counter =  (letEnv, localEnv)

--------------------Helpers--------------------
makeapp a = a ++ [Makeapp]
push op = [Pushfun op, Makeapp]

isInEnv a ((x,y):xs) = (a == x) || isInEnv a xs
isInEnv a [] = False

getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 0

incrementPos ([], b) = ([], increment b)
incrementPos (a, b) = (incrementLetEnv a (length a), b)

increment = map(\(x,y) -> (x,y+1))

incrementLetEnv localEnv@((x,y):xs) counter = (x,y - counter) : incrementLetEnv xs (counter+1) 
incrementLetEnv [] counter = [] 

--------------------Test Cases--------------------
testProg2 = [FuncDef "a" ["a", "b"] (PlusX (VarX "a") (VarX "b"))]
testProg8 = [VarDef "a" (IfX (BoolX True) (IntX 1) (IntX 2))] ++ testProg2
testProg = [VarDef "a" (BoolX True), VarDef "b" (IntX 2)]
testProg3 = [VarDef "a" (IntX 1) ,VarDef "b" (PlusX (VarX "a") (IntX 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (AndX (BoolX True) (BoolX False))]
testProg5 = [VarDef "x" ( LetX [LocDef "a" (IntX 8), LocDef "b" (IntX 8)] (PlusX (VarX "a") (VarX "b")))]


{--


(+ a (+ b c)) 

üb(+ b c)
üb(+ a)  (Pos + 1)
makeapp

üb(c)
üb(+ b) (Pos + 1)
makeapp
üb(+ a) 
makeapp

üb(c)
üb(b)
üb(+) (Pos + 1)
makeapp
makeapp
üb(+ a) 
makeapp

üb(c) -1
üb(b) 1 
üb(+)
makeapp
makeapp
üb(a) 2 
üb(+) (Pos + 1)
makeapp
makeapp1
2 1 -1 

Pre übkonsLet : [a = 1 (2), b = 2 (3), c = 3 (3)]

üb (a) (e1, Pos) 2
Alloc
Makeapp 
üb (b) (e2, (Pos+1)[v1 -> -1]) 1
Alloc
Makeapp
üb (c) (e2, (Pos+2)[v1 -> 0 v2 -> -1]) -1
Alloc
Makeapp

--}