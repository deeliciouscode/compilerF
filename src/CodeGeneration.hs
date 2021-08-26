module CodeGeneration where
import Data.Maybe
import Data.Map
import Parser
-- import Relude
import Data.Text
import Helpers
import Lexer
import DataStructures
import Instructions


translateProg xs = Prelude.foldr (\ x -> (++) (translateDef x [])) [] xs


translateDef :: Def -> [Instructions] -> [Instructions]
translateDef def list =
    case def of
        VarDef name expr -> translateVar name expr list
        FuncDef name args expr -> translateFunc name args expr list (createLocalEnv args 0)
        Deps -> list

translateVar :: String -> Expr -> [Instructions] -> [Instructions]
translateVar name expr list = translateExpr expr [] ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list 

translateFunc name args expr list localEnv = translateExpr expr localEnv ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return] ++ list

createLocalEnv (x:xs) counter = (x,counter+1) : createLocalEnv xs (counter+1)
createLocalEnv [] _ = []

isInLocalEnv a ((x,y):xs) = (a == x) || isInLocalEnv a xs
isInLocalEnv a [] = False

getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 0

translateExpr :: Expr -> [(String, Int)] -> [Instructions]
translateExpr expr localEnv = 
    case expr of
        Var a -> if isInLocalEnv a localEnv then [Pushparam (getPos a localEnv)] else [Pushfun a]
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushfun (show a)]
        (Plus expr1 expr2) -> translatePlus (Plus expr1 expr2) localEnv
        (Minus expr expr2) -> translateMinus (Minus expr expr2) localEnv
        (Mult expr expr2) -> translateMult (Mult expr expr2) localEnv
        (Div expr1 expr2) -> translateDiv (Div expr1 expr2) localEnv
        (Or expr expr2) -> translateOr (Or expr expr) localEnv 
        (And expr expr2) -> translateAnd (And expr expr2) localEnv
        (Smaller expr expr2) -> translateSmaller (Smaller expr expr2) localEnv
        (Equals expr expr2) -> translateEquals (Equals expr expr2) localEnv
        (Not expr) -> translateExpr expr localEnv ++ [Pushfun "Not", Makeapp] 
        (Neg expr) -> translateExpr expr localEnv ++ [Pushfun "Negate", Makeapp]
        (If expr1 expr2 expr3) -> translateIf (If expr1 expr2 expr3) localEnv
        (Let locdefs expr) -> translateLet locdefs expr (createLetEnv locDefs localEnv)
        

--translateLet (x:xs) expr localLetEnv = 

updatePos = Prelude.map (\(x,y) -> (x,y+1))

translatePlus (Plus expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "+", Makeapp]
translatePlus (Plus expr1 expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (Plus expr1 EmptyExpr) (updatePos localEnv) ++ [Makeapp]
        
translateMinus (Minus expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "+", Makeapp]
translateMinus (Minus expr1 expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (Minus expr1 EmptyExpr) (updatePos localEnv) ++ [Makeapp]

translateMult (Mult expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "*", Makeapp]
translateMult (Mult expr1 expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (Mult expr1 EmptyExpr) (updatePos localEnv) ++ [Makeapp]

translateDiv (Div expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "/", Makeapp]
translateDiv (Div expr1 expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (Div expr1 EmptyExpr) (updatePos localEnv) ++ [Makeapp]

translateOr (Or expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "|", Makeapp]
translateOr (Or expr expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (Or expr EmptyExpr) (updatePos localEnv) ++ [Makeapp]

translateAnd (And expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "&", Makeapp]
translateAnd (And expr expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (And expr EmptyExpr) (updatePos localEnv) ++ [Makeapp]

translateSmaller (Smaller expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "<", Makeapp]
translateSmaller (Smaller expr expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (Smaller expr EmptyExpr) (updatePos localEnv) ++ [Makeapp]

translateEquals (Equals expr EmptyExpr) localEnv = translateExpr expr localEnv ++ [Pushfun "==", Makeapp]
translateEquals (Equals expr expr2) localEnv = translateExpr expr2 localEnv ++ translateExpr (Equals expr EmptyExpr) (updatePos localEnv) ++ [Makeapp]

translateIf (If expr1 EmptyExpr EmptyExpr) localEnv = translateExpr expr1 localEnv ++ [Pushfun "if"] ++ [Makeapp]
translateIf (If expr1 expr2 EmptyExpr) localEnv = translateExpr expr2 localEnv ++ translateExpr (If expr1 EmptyExpr EmptyExpr) (updatePos localEnv)  ++ [Makeapp]
translateIf (If expr1 expr2 expr3) localEnv = translateExpr expr3 localEnv ++ translateExpr (If expr1 expr2 EmptyExpr) (updatePos localEnv) ++ [Makeapp]

testProg2 = [FuncDef "a" ["a", "b"] (Plus (Var "a") (Var "b"))]
testProg8 = [VarDef "a" (If (Bool True) (Int 1) (Int 2))] ++ testProg2



