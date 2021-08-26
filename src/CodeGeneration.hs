{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CodeGeneration where
import Data.Maybe
import Data.Map

import Parser
-- import Relude
import Data.Text

import Helpers
import Lexer
import DataStructures


--translateProg xs = Prelude.foldr (\ x -> (++) (translateDef x [])) [] xs

translateDef :: Def -> [Instructions] -> [Instructions]
translateDef def list =
    case def of
        VarDef name expr -> translateVar name expr list
        FuncDef name args expr -> translateFunc name args expr list
        Deps -> list

translateVar :: String -> Expr -> [Instructions] -> [Instructions]
translateVar name expr list = translateExpr expr ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list 

translateFunc :: String -> [Arg] -> Expr -> [Instructions] -> [Instructions]
translateFunc name args expr list = Pushfun name : translateArgs args ++ translateExpr expr ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list

translateExpr expr = 
    case expr of
        Var a -> [Pushfun a]
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushfun (show a)]
        (Plus expr1 expr2) -> translatePlus (Plus expr1 expr2)
        (Minus expr expr2) -> translateMinus (Minus expr expr2)
        (Mult expr expr2) -> translateMult (Mult expr expr2)
        (Div expr1 expr2) -> translateDiv (Div expr1 expr2)
        (Or expr expr2) -> translateOr (Or expr expr)
        (And expr expr2) -> translateAnd (And expr expr2)
        (Smaller expr expr2) -> translateSmaller (Smaller expr expr2)
        (Equals expr expr2) -> translateEquals (Equals expr expr2)
        (Not expr) -> translateExpr expr ++ [Pushfun "Not", Makeapp]
        (Neg expr) -> translateExpr expr ++ [Pushfun "Negate", Makeapp]
        (If expr1 expr2 expr3) -> translateIf (If expr1 expr2 expr3)
        
translatePlus (Plus expr EmptyExpr) = translateExpr expr ++ [Pushfun "+", Makeapp]
translatePlus (Plus expr1 expr2) = translateExpr expr2 ++ translateExpr (Plus expr1 EmptyExpr) ++ [Makeapp]
        
translateMinus (Minus expr EmptyExpr) = translateExpr expr ++ [Pushfun "+", Makeapp]
translateMinus (Minus expr1 expr2) = translateExpr expr2 ++ translateExpr (Minus expr1 EmptyExpr) ++ [Makeapp]

translateMult (Mult expr EmptyExpr) = translateExpr expr ++ [Pushfun "*", Makeapp]
translateMult (Mult expr1 expr2) = translateExpr expr2 ++ translateExpr (Mult expr1 EmptyExpr) ++ [Makeapp]

translateDiv (Div expr EmptyExpr) = translateExpr expr ++ [Pushfun "/", Makeapp]
translateDiv (Div expr1 expr2) = translateExpr expr2 ++ translateExpr (Div expr1 EmptyExpr) ++ [Makeapp]

translateOr (Or expr EmptyExpr) = translateExpr expr ++ [Pushfun "|", Makeapp]
translateOr (Or expr expr2) = translateExpr expr2 ++ translateExpr (Or expr EmptyExpr) ++ [Makeapp]

translateAnd (And expr EmptyExpr) = translateExpr expr ++ [Pushfun "&", Makeapp]
translateAnd (And expr expr2) = translateExpr expr2 ++ translateExpr (And expr EmptyExpr) ++ [Makeapp]

translateSmaller (Smaller expr EmptyExpr) = translateExpr expr ++ [Pushfun "<", Makeapp]
translateSmaller (Smaller expr expr2) = translateExpr expr2 ++ translateExpr (Smaller expr EmptyExpr) ++ [Makeapp]

translateEquals (Equals expr EmptyExpr) = translateExpr expr ++ [Pushfun "==", Makeapp]
translateEquals (Equals expr (Bool a)) = translateExpr (Bool a) ++ translateExpr (Equals expr EmptyExpr) ++ [Makeapp]
translateEquals (Equals expr (Int a)) = translateExpr (Int a) ++ translateExpr (Equals expr EmptyExpr) ++ [Makeapp]

translateIf (If expr1 EmptyExpr EmptyExpr) = translateExpr expr1 ++ [Pushfun "if"] ++ [Makeapp]
translateIf (If expr1 expr2 EmptyExpr) = translateExpr expr2 ++ translateExpr (If expr1 EmptyExpr EmptyExpr) ++ [Makeapp]
translateIf (If expr1 expr2 expr3) = translateExpr expr3 ++ translateExpr (If expr1 expr2 EmptyExpr) ++ [Makeapp]

translateProg (x:xs) = translateDef x [] ++ translateProg xs
translateProg [] = []

testProg2 = [VarDef "a" (Plus (Int 1) (Int 2))]
testProg8 = [VarDef "a" (If (Bool True) (Int 1) (Int 2))] ++ testProg2



translateLocDefs :: [LocDef] -> [Instructions]
translateLocDefs ((LocDef name expr):xs) = translateLocDefs xs ++ translateExpr expr ++ Pushfun name : []
translateLocDefs [] = []


translateArgs :: [Arg] -> [Instructions]
translateArgs args = translateLocalEnv (createLocalEnv args [])

createLocalEnv (x:xs) list =  (x, Prelude.length (x:xs)) : createLocalEnv xs list
createLocalEnv [] _  = []

translateLocalEnv = Prelude.map (Pushparam . snd)

--- Test Cases ---

testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]
testProg5 = [VarDef "x" (Let [LocDef "a" (Int 8), LocDef "b" (Int 13)] (Plus (Var "a") (Var "b")))]


