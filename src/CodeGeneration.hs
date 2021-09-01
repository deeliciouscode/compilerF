{-# LANGUAGE ViewPatterns #-}
module CodeGeneration where

import Parser
import Helpers
import Lexer
import DataStructures
import Instructions

-------------------------- Main Functions --------------------------
main :: Ast -> (GlobalEnvironment, Code)
main ast = (generateDefList ast, generateCode ast)

generateDefList :: Ast -> GlobalEnvironment
generateDefList ast = setIndices 4 $ astToDefList ast ++ initDef

generateCode :: Ast -> Code
generateCode ast =  concatInstructions $ concatMap translateTree ast

concatInstructions code = mHead ++ code ++ mTail

-------------------------- Global Environment --------------------------
-- createDefCell :: String -> Int -> Int -> DefCell
createDefCell name arity instructions =  (name, (arity, length instructions))

generateDef :: SubTree -> DefCell
generateDef (VarDef name expr) = createDefCell name 0 $ translateVar name expr (createLocalEnv [] 1)
generateDef (FuncDef name args expr) = createDefCell name (length args) $ translateFunc name args expr initLocEnv

setIndices :: Int -> [DefCell] -> GlobalEnvironment
setIndices n [(a,(x,y))] = [(a,(x,n))]
setIndices n ((a,(x,y)):xs) = (a,(x,n)) : setIndices (y+n) xs

astToDefList :: Ast -> [DefCell]
astToDefList = map generateDef

-------------------------- Translation --------------------------
translateTree :: SubTree -> [Instructions]
translateTree subtree  =
    case subtree of
        VarDef name expr -> translateVar name expr (createLocalEnv [] 0)
        FuncDef name args expr -> translateFunc name args expr (createLocalEnv args 0)

translateVar :: String -> Expr -> LocalEnvironment -> [Instructions]
translateVar name expr locEnv = translExpr expr locEnv ++ varTail

translateFunc :: String -> Args -> Expr -> LocalEnvironment -> [Instructions]
translateFunc name args expr localEnv = translExpr expr localEnv ++ functionTail (length args)

translAndInc :: Expr -> LocalEnvironment -> [Instructions]
translAndInc expr env = translExpr expr (increment env)

translExpr :: Expr -> LocalEnvironment -> [Instructions]
translExpr expr locEnv =
    case expr of
        VarX a -> if isInEnv a locEnv then [Pushparam (getPos a locEnv)] else [Pushfun  a]
        IntX a -> [Pushval (IntX a)]
        BoolX a -> [Pushfun (show a)]
        e@(PlusX expr1 expr2) -> translatePlus e locEnv
        e@(MinusX expr expr2) -> translateMinus e locEnv
        e@(MultX expr expr2) -> translateMult e locEnv
        e@(DivX expr1 expr2) -> translateDiv e locEnv
        e@(OrX expr expr2) -> translateOr e locEnv 
        e@(AndX expr expr2) -> translateAnd e locEnv
        e@(SmallerX expr expr2) -> translateSmaller e locEnv
        e@(EqualsX expr expr2) -> translateEquals e locEnv
        (NotX expr) -> translExpr expr locEnv ++ push "Not"
        (NegX expr) -> translExpr expr locEnv ++ push "Negate"
        e@(IfX expr1 expr2 expr3) -> translateIf e locEnv
        (LetX locDefs expr) -> translLocDefs locDefs locEnv (-1) ++ translLetExpr expr locDefs locEnv (-1)
        e@(AppX expr1 expr2) -> translAppExpr e locEnv

translLocDefs [] _ _ = []
translLocDefs locdefs@((LocDef name expr):xs) localEnv i@counter = translExpr expr (updateLocalVarValue (reverse locdefs) localEnv i) ++ allocMake ++ translLocDefs xs (updateLocalVarValue (reverse locdefs) localEnv i) i

translLetExpr expr locDefs localEnv i@counter = translExpr expr ((addLetEnv (reverse (locDefs)) localEnv i)) ++ slideLet (length locDefs)

translatePlus (PlusX expr EmptyExpr) env = translExpr expr env ++ push "+"
translatePlus (PlusX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (PlusX expr1 EmptyExpr) env
        
translateMinus (MinusX expr EmptyExpr) env = translExpr expr env ++ push"-"
translateMinus (MinusX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (MinusX expr1 EmptyExpr) env

translateMult (MultX expr EmptyExpr) env = translExpr expr env ++ push "*"
translateMult (MultX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (MultX expr1 EmptyExpr) env

translateDiv (DivX expr EmptyExpr) env = translExpr expr env ++ push "/"
translateDiv (DivX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (DivX expr1 EmptyExpr) env

translateOr (OrX expr EmptyExpr) env = translExpr expr env ++ push "|"
translateOr (OrX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (OrX expr EmptyExpr) env

translateAnd (AndX expr EmptyExpr) env = translExpr expr env ++ push "&"
translateAnd (AndX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (AndX expr EmptyExpr) env

translateSmaller (SmallerX expr EmptyExpr) env = translExpr expr env ++ push "<"
translateSmaller (SmallerX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (SmallerX expr EmptyExpr)  env

translateEquals (EqualsX expr EmptyExpr) env = translExpr expr env ++ push "=="
translateEquals (EqualsX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (EqualsX expr EmptyExpr) env

translateIf (IfX expr1 EmptyExpr EmptyExpr) env = translExpr expr1 env ++ push "if"
translateIf (IfX expr1 expr2 EmptyExpr) env = makeapp $ translExpr expr2 env ++ translAndInc (IfX expr1 EmptyExpr EmptyExpr) env
translateIf (IfX expr1 expr2 expr3) env = makeapp $ translExpr expr3 env ++ translAndInc (IfX expr1 expr2 EmptyExpr) env

translAppExpr (AppX expr1 expr2) locEnv = makeapp $ translExpr expr2 locEnv ++ translExpr expr1 locEnv

--------------------Local Environments--------------------
createLocalEnv (x:xs) i@counter = (x,i+1) : createLocalEnv xs (i+1)
createLocalEnv [] _ = []

addLetEnv ((LocDef name expr):xs) localEnv i@counter =  addLetEnv xs (localEnv ++ [(name, i)]) (i+1)
addLetEnv [] localEnv counter = localEnv

updateLocalVarValue localDef localEnv i@counter = if length localDef > 1 then addLetEnv localDef localEnv i else localEnv

initLocEnv = createLocalEnv [] 0

isInEnv a ((x,y):xs) = a == x || isInEnv a xs 
isInEnv a [] = False

getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 4

increment = map(\(x,y) -> (x,y+1))

--------------------Test Cases--------------------
testProg2 = [FuncDef "f" ["a", "b"] (PlusX (VarX "a") (VarX "b"))]
testProg8 = [VarDef "a" (IfX (BoolX True) (IntX 1) (IntX 2))] 
testProg = [VarDef "a" (BoolX True), VarDef "b" (IntX 2)]
testProg3 = [VarDef "a" (IntX 1) ,VarDef "b" (PlusX (VarX "a") (IntX 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (AndX (BoolX True) (BoolX False))]
testProg5 = [VarDef "f" ( LetX [LocDef "a" (IntX 1)] (VarX "a") )]
testProg6 = [VarDef "f" (LetX [LocDef "a" (IntX 1), LocDef "b" (IntX 2)] (PlusX (VarX "a") (VarX "b"))) ]
testProg7 = [VarDef "f" (LetX [LocDef "a" (IntX 1), LocDef "b" (IntX 2), LocDef "c" (IntX 3)] (PlusX (VarX "a") (PlusX (VarX "b") (VarX "c"))))]
testProg9 = [FuncDef "id" ["a"] (VarX "a"), FuncDef "main" [] (AppX (VarX "id") (IntX 0))]
