{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
        -- Var a -> if isInEnv a letEnv || isInEnv a locEnv then [Pushparam (getPos a (letEnv, locEnv)] else [Pushfun a]
        Var a -> if isInEnv a letEnv then [Pushparam (getPos a letEnv)] else if isInEnv a locEnv then [Pushparam (getPos a locEnv)] else [Pushfun a]
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
        (Let locdefs expr) -> translateLet locdefs expr (createLetEnv locdefs ([],locEnv) 1) []

translateLet [] EmptyExpr env@(letEnv, localEnv) instructions = instructions ++ [SlideLet (length letEnv)]
translateLet [] expr env@(letEnv, localEnv) instructions = instructions ++ translateExpr expr env
translateLet locdefs expr env@(letEnv, localEnv) [] = translateLetExpr locdefs env [] expr

translateLetExpr [] env@(letEnv, localEnv) instructions globalExpr = translateLet [] EmptyExpr env (instructions ++ translateExpr globalExpr env) 
translateLetExpr ((LocDef name expr):xs) env@(letEnv, localEnv) instructions globalExpr =  translateLetExpr xs (incrementPos env) (translateExpr expr env ++ [Alloc, Makeapp] ++ instructions) globalExpr

translatePlus (Plus expr EmptyExpr) env = translateExpr expr env ++ push "+"
translatePlus (Plus expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Plus expr1 EmptyExpr) (incrementPos env)
        
translateMinus (Minus expr EmptyExpr) env = translateExpr expr env ++ push"-"
translateMinus (Minus expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Minus expr1 EmptyExpr) (incrementPos env)

translateMult (Mult expr EmptyExpr) env = translateExpr expr env ++ push "*"
translateMult (Mult expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Mult expr1 EmptyExpr) (incrementPos env)

translateDiv (Div expr EmptyExpr) env = translateExpr expr env ++ push "/"
translateDiv (Div expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Div expr1 EmptyExpr) (incrementPos env)

translateOr (Or expr EmptyExpr) env = translateExpr expr env ++ push "|"
translateOr (Or expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Or expr EmptyExpr) (incrementPos env)

translateAnd (And expr EmptyExpr) env = translateExpr expr env ++ push "&"
translateAnd (And expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (And expr EmptyExpr) (incrementPos env)

translateSmaller (Smaller expr EmptyExpr) env = translateExpr expr env ++ push "<"
translateSmaller (Smaller expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Smaller expr EmptyExpr) (incrementPos env)

translateEquals (Equals expr EmptyExpr) env = translateExpr expr env ++ push "=="
translateEquals (Equals expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Equals expr EmptyExpr) (incrementPos env)

translateIf (If expr1 EmptyExpr EmptyExpr) env = translateExpr expr1 env ++ push "if"
translateIf (If expr1 expr2 EmptyExpr) env = makeapp $ translateExpr expr2 env ++ translateExpr (If expr1 EmptyExpr EmptyExpr) (incrementPos env)
translateIf (If expr1 expr2 expr3) env = makeapp $ translateExpr expr3 env ++ translateExpr (If expr1 expr2 EmptyExpr) (incrementPos env)

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
testProg2 = [FuncDef "a" ["a", "b"] (Plus (Var "a") (Var "b"))]
testProg8 = [VarDef "a" (If (Bool True) (Int 1) (Int 2))] ++ testProg2
testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]
testProg5 = [VarDef "x" ( Let [LocDef "a" (Int 8), LocDef "b" (Int 8)] (Plus (Var "a") (Var "b")))]
