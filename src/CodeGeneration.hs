{-# LANGUAGE ViewPatterns #-}
module CodeGeneration where

import Parser
import Helpers
import Lexer
import DataStructures
import Instructions
import qualified Data.Sequence as Seq

-------------------------- Main Functions --------------------------
main :: Ast -> (GlobalEnvironment, Code)
main ast = (generateDefList ast, generateCode ast)

generateDefList :: Ast -> GlobalEnvironment
generateDefList ast = setIndices 4 $ astToDefList ast ++ initDef

generateCode :: Ast -> Code
generateCode ast =  concatInstructions $ concatMap translateTree ast

concatInstructions code = mHead ++ code ++ mTail

-------------------------- Global Environment --------------------------
createDefCell :: String -> Int -> Int -> DefCell
createDefCell name arity length =  (name, (arity, length))

generateDef :: SubTree -> DefCell
generateDef (VarDef name expr) = createDefCell name 0 (length $ translateVar name expr (createLocalEnv [] 1)) 
generateDef (FuncDef name args expr) = createDefCell name (length args) (length (translateFunc name args expr (createLocalEnv [] 0)))

setIndices :: Int -> [DefCell] -> GlobalEnvironment
setIndices n [(a,(x,y))] = [(a,(x,n))]
setIndices n ((a,(x,y)):xs) = (a,(x,n)) : setIndices (y+n) xs

astToDefList :: Ast -> [DefCell]
astToDefList = map generateDef

-------------------------- Translation --------------------------
translateTree :: SubTree -> [Instructions]
translateTree subtree  =
    case subtree of
        VarDef name expr -> translateVar name expr $ createLocalEnv [] 0
        FuncDef name args expr -> translateFunc name args expr $ createLocalEnv [] 0

translateVar :: String -> Expr -> LocalEnvironment -> [Instructions]
translateVar name expr locEnv = evalExpr expr locEnv ++ [Update 0, Slide 1, Unwind, Call, Return]

translateFunc :: String -> Args -> Expr -> LocalEnvironment -> [Instructions]
translateFunc name args expr localEnv = evalExpr expr localEnv ++ [Update (length args), Slide (length args + 1), Unwind, Call, Return]

evalExpr :: Expr -> LocalEnvironment -> [Instructions]
evalExpr expr locEnv = 
    case expr of
        VarX a -> if isInEnv a locEnv then [Pushparam (getPos a locEnv)] else [Pushfun a] 
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
        (NotX expr) -> evalExpr expr locEnv ++ push "Not"
        (NegX expr) -> evalExpr expr locEnv ++ push "Negate"
        e@(IfX expr1 expr2 expr3) -> translateIf e locEnv
        (LetX locDefs expr) -> evalLocalDefs locDefs locEnv (-1) ++ evalLetExpr expr locDefs locEnv (-1)

evalLocalDefs [] _ _ = [] 
evalLocalDefs locdefs@((LocDef name expr):xs) localEnv i@counter = evalExpr expr (updateLocalVarValue (reverse locdefs) localEnv i) ++ [Alloc, Makeapp] ++ evalLocalDefs xs (updateLocalVarValue (reverse locdefs) localEnv i) i

evalLetExpr expr locDefs localEnv i@counter = evalExpr expr ((addLetEnv (reverse (locDefs)) localEnv i)) ++ [SlideLet (length locDefs)]

translatePlus (PlusX expr EmptyExpr) env = evalExpr expr env ++ push "+"
translatePlus (PlusX expr1 expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (PlusX expr1 EmptyExpr) (increment env)
        
translateMinus (MinusX expr EmptyExpr) env = evalExpr expr env ++ push"-"
translateMinus (MinusX expr1 expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (MinusX expr1 EmptyExpr) (increment env)

translateMult (MultX expr EmptyExpr) env = evalExpr expr env ++ push "*"
translateMult (MultX expr1 expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (MultX expr1 EmptyExpr) (increment env)

translateDiv (DivX expr EmptyExpr) env = evalExpr expr env ++ push "/"
translateDiv (DivX expr1 expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (DivX expr1 EmptyExpr) (increment env)

translateOr (OrX expr EmptyExpr) env = evalExpr expr env ++ push "|"
translateOr (OrX expr expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (OrX expr EmptyExpr) (increment env)

translateAnd (AndX expr EmptyExpr) env = evalExpr expr env ++ push "&"
translateAnd (AndX expr expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (AndX expr EmptyExpr) (increment env)

translateSmaller (SmallerX expr EmptyExpr) env = evalExpr expr env ++ push "<"
translateSmaller (SmallerX expr expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (SmallerX expr EmptyExpr) (increment env)

translateEquals (EqualsX expr EmptyExpr) env = evalExpr expr env ++ push "=="
translateEquals (EqualsX expr expr2) env = makeapp $ evalExpr expr2 env ++ evalExpr (EqualsX expr EmptyExpr) (increment env)

translateIf (IfX expr1 EmptyExpr EmptyExpr) env = evalExpr expr1 env ++ push "if"
translateIf (IfX expr1 expr2 EmptyExpr) env = makeapp $ evalExpr expr2 env ++ evalExpr (IfX expr1 EmptyExpr EmptyExpr) (increment env)
translateIf (IfX expr1 expr2 expr3) env = makeapp $ evalExpr expr3 env ++ evalExpr (IfX expr1 expr2 EmptyExpr) (increment env)

--------------------Local Environments--------------------
createLocalEnv (x:xs) i@counter = (x,i+1) : createLocalEnv xs (i+1)
createLocalEnv [] _ = []

addLetEnv ((LocDef name expr):xs) localEnv i@counter =  addLetEnv xs (localEnv ++ [(name, i)]) (i+1)
addLetEnv [] localEnv counter = localEnv

updateLocalVarValue localDef localEnv i@counter = if length localDef > 1 then addLetEnv localDef localEnv i else localEnv

--------------------Helpers--------------------
makeapp a = a ++ [Makeapp]
push op = [Pushfun op, Makeapp]

isInEnv a ((x,y):xs) = a == x || isInEnv a xs 
isInEnv a [] = False

getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 4

increment = map(\(x,y) -> (x,y+1))

initDef :: [DefCell]
initDef = 
    [ ("false",(0,6))
    , ("true",(0,6))
    , ("not",(1,7))
    , ("negate",(1,7))
    , ("|",(2,10))
    , ("&",(2,10))
    , ("+",(2,10))
    , ("-",(2,10))
    , ("*",(2,10))
    , ("/",(2,10))
    , ("==",(2,10))
    , ("<",(2,10))
    , ("if", (3,11))
    ]

--------------------Test Cases--------------------
testProg2 = [FuncDef "f" ["a", "b"] (PlusX (VarX "a") (VarX "b"))]
testProg8 = [VarDef "a" (IfX (BoolX True) (IntX 1) (IntX 2))] 
testProg = [VarDef "a" (BoolX True), VarDef "b" (IntX 2)]
testProg3 = [VarDef "a" (IntX 1) ,VarDef "b" (PlusX (VarX "a") (IntX 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (AndX (BoolX True) (BoolX False))]
testProg5 = [VarDef "f" ( LetX [LocDef "a" (IntX 1)] (VarX "a") )]
testProg6 = [VarDef "f" (LetX [LocDef "a" (IntX 1), LocDef "b" (IntX 2)] (PlusX (VarX "a") (VarX "b"))) ]
testProg7 = [VarDef "f" (LetX [LocDef "a" (IntX 1), LocDef "b" (IntX 2), LocDef "c" (IntX 3)] (PlusX (VarX "a") (PlusX (VarX "b") (VarX "c")))) ]
