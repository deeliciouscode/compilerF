{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}
module CodeGeneration where
import Data.Maybe
import Parser
import Helpers
import Lexer
import DataStructures
import Instructions
import qualified Data.Sequence as Seq


-------------------------- Translation--------------------------
translateProg (x:xs) = translateDef x [] ++ translateProg xs
translateProg [] = []

translateDef def locEnv =
    case def of
        VarDef name expr -> translateVar name expr (Seq.fromList $ (createLocalEnv [] 0)) 
        FuncDef name args expr -> translateFunc name args expr (Seq.fromList $ (createLocalEnv args 0))

translateVar name expr localEnv  = translateExpr expr localEnv ++ [Update 0, Slide 1, Unwind, Call, Return]

translateFunc name args expr localEnv = translateExpr expr localEnv ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return]

translateExpr expr locEnv = 
    case expr of
        Var a -> if isInEnv a locEnv then [Pushparam (getPos a locEnv)] else [Pushfun a]
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushfun (show a)]
        e@(Plus expr1 expr2) -> translatePlus e locEnv
        e@(Minus expr expr2) -> translateMinus e locEnv
        e@(Mult expr expr2) -> translateMult e locEnv
        e@(Div expr1 expr2) -> translateDiv e locEnv
        e@(Or expr expr2) -> translateOr e locEnv 
        e@(And expr expr2) -> translateAnd e locEnv
        e@(Smaller expr expr2) -> translateSmaller e locEnv
        e@(Equals expr expr2) -> translateEquals e locEnv
        (Not expr) -> translateExpr expr locEnv ++ push "Not"
        (Neg expr) -> translateExpr expr locEnv ++ push "Negate"
        e@(If expr1 expr2 expr3) -> translateIf e locEnv
        (Let locdefs expr) -> translateLocalDefs locdefs locEnv (-1) ++ translateLetExpr expr locDefs locEnv


translateLocalDefs [] _ _ = [] 
translateLocalDefs locDefs@((LocDef name expr):xs) localEnv counter = translateExpr expr (updateLocalVarValue (reverse locDefs) localEnv counter) ++ [Alloc, Makeapp] ++ translateLocalDefs xs (updateLocalVarValue (reverse locDefs) localEnv counter) counter

updateLocalVarValue localDef localEnv counter = if length localDef > 1 then addLetEnv localDef localEnv counter else localEnv


addLetEnv :: [LocDef] -> Seq.Seq (String, Int) -> Int -> Seq.Seq (String, Int)
addLetEnv [x] localEnv counter = localEnv
addLetEnv ((LocDef name expr):xs) localEnv counter =  increment $ localEnv Seq.>< addLetEnv xs (localEnv Seq.>< Seq.singleton(name, counter)) (counter+1)


translateLet EmptyExpr localEnv instructions = instructions ++ [SlideLet (length localEnv)]

-- translateLet [] expr localEnv instructions = instructions ++ translateExpr expr localEnv

-- translateLetExpr [] localEnv instructions globalExpr = translateLet [] EmptyExpr localEnv (instructions ++ translateExpr globalExpr localEnv) 
-- translateLetExpr ((LocDef name expr):xs) localEnv instructions globalExpr =  translateLetExpr xs (incrementPos localEnv) (translateExpr expr localEnv ++ [Alloc, Makeapp] ++ instructions) globalExpr


translatePlus (Plus expr EmptyExpr) env = translateExpr expr env ++ push "+"
translatePlus (Plus expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Plus expr1 EmptyExpr) (increment env)
        
translateMinus (Minus expr EmptyExpr) env = translateExpr expr env ++ push"-"
translateMinus (Minus expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Minus expr1 EmptyExpr) (increment env)

translateMult (Mult expr EmptyExpr) env = translateExpr expr env ++ push "*"
translateMult (Mult expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Mult expr1 EmptyExpr) (increment env)

translateDiv (Div expr EmptyExpr) env = translateExpr expr env ++ push "/"
translateDiv (Div expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Div expr1 EmptyExpr) (increment env)

translateOr (Or expr EmptyExpr) env = translateExpr expr env ++ push "|"
translateOr (Or expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Or expr EmptyExpr) (increment env)

translateAnd (And expr EmptyExpr) env = translateExpr expr env ++ push "&"
translateAnd (And expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (And expr EmptyExpr) (increment env)

translateSmaller (Smaller expr EmptyExpr) env = translateExpr expr env ++ push "<"
translateSmaller (Smaller expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Smaller expr EmptyExpr) (increment env)

translateEquals (Equals expr EmptyExpr) env = translateExpr expr env ++ push "=="
translateEquals (Equals expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (Equals expr EmptyExpr) (increment env)

translateIf (If expr1 EmptyExpr EmptyExpr) env = translateExpr expr1 env ++ push "if"
translateIf (If expr1 expr2 EmptyExpr) env = makeapp $ translateExpr expr2 env ++ translateExpr (If expr1 EmptyExpr EmptyExpr) (increment env)
translateIf (If expr1 expr2 expr3) env = makeapp $ translateExpr expr3 env ++ translateExpr (If expr1 expr2 EmptyExpr) (increment env)

--------------------Environments--------------------
createLocalEnv (x:xs) counter = (x,counter+1) : createLocalEnv xs (counter+1)
createLocalEnv [] _ = []

--------------------Helpers--------------------
makeapp a = a ++ [Makeapp]
push op = [Pushfun op, Makeapp]


isInEnv a (Seq.viewl -> (x,y) Seq.:< xs) = a == x || isInEnv a xs 
isInEnv a (Seq.viewl -> Seq.EmptyL)  = False

getPos a (Seq.viewl -> (x,y) Seq.:< xs) = if a == x then y else getPos a xs
getPos a (Seq.viewl -> Seq.EmptyL) = 0
 
increment = Seq.mapWithIndex (\int (x,y) -> (x,y+1))

--------------------Test Cases--------------------
testProg2 = [FuncDef "f" ["a", "b"] (Plus (Var "a") (Var "b"))]
testProg8 = [VarDef "a" (If (Bool True) (Int 1) (Int 2))] ++ testProg2
testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]
testProg5 = [VarDef "x" ( Let [LocDef "a" (Int 8), LocDef "b" (Int 8)] (Plus (Var "a") (Var "b")))]
