{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}
module CodeGeneration where

import Parser
import Helpers
import Lexer
import DataStructures
import Instructions
import qualified Data.Sequence as Seq

-------------------------- Main Functions --------------------------
generateDefList :: Ast -> DefList
generateDefList ast = setIndices 0 $ astToDefList ast ++ initDef

generateCode :: Ast -> Code
generateCode (x:xs) =  translateTree x ++ generateCode xs

-------------------------- Global Environment --------------------------
initDef :: DefList
initDef = 
    [ ("false",(1,6))
    , ("true",(1,6))
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

createDefCell :: String -> Int -> Int -> DefCell
createDefCell name arity length =  (name, (arity, length))

generateDefCell :: SubTree -> DefCell
generateDefCell (VarDef name expr) = createDefCell name 0 (length $ translateVar name expr (Seq.fromList $ createLocalEnv [] 1)) 
generateDefCell (FuncDef name args expr) = createDefCell name (length args) (length (translateFunc name args expr (Seq.fromList $ createLocalEnv [] 0)))

setIndices :: Int -> DefList -> DefList
setIndices n ((a,(x,y)):[]) = [(a,(x,n))]
setIndices n ((a,(x,y)):xs) = (a,(x,n)) : setIndices (y+n) xs

astToDefList :: Ast -> DefList
astToDefList = map generateDefCell

-------------------------- Translation --------------------------

translateTree :: SubTree -> Code
translateTree subtree  =
    case subtree of
        VarDef name expr -> translateVar name expr $ Seq.fromList $ createLocalEnv [] 0
        FuncDef name args expr -> translateFunc name args expr $ Seq.fromList $ createLocalEnv [] 0

translateVar :: String -> Expr -> Seq.Seq (String, Int) -> Code
translateVar name expr locEnv = translateExpr expr locEnv ++ [Update 0, Slide 1, Unwind, Call, Return]

translateFunc name args expr localEnv = translateExpr expr localEnv ++ [Update (length args), Slide (length args + 1), Unwind, Call, Return]

translateExpr expr locEnv = 
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
        (NotX expr) -> translateExpr expr locEnv ++ push "Not"
        (NegX expr) -> translateExpr expr locEnv ++ push "Negate"
        e@(IfX expr1 expr2 expr3) -> translateIf e locEnv
        (LetX locdefs expr) -> translateLocalDefs locdefs locEnv (-1) -- ++ translateLetExpr expr locdefs locEnv


translateLocalDefs [] _ _ = [] 
translateLocalDefs locDefs@((LocDef name expr):xs) localEnv counter = translateExpr expr (updateLocalVarValue (reverse locDefs) localEnv counter) ++ [Alloc, Makeapp] ++ translateLocalDefs xs (updateLocalVarValue (reverse locDefs) localEnv counter) counter

updateLocalVarValue localDef localEnv counter = if length localDef > 1 then addLetEnv localDef localEnv counter else localEnv


addLetEnv :: [LocDef] -> Seq.Seq (String, Int) -> Int -> Seq.Seq (String, Int)
addLetEnv [x] localEnv counter = localEnv
addLetEnv ((LocDef name expr):xs) localEnv counter =  increment $ localEnv Seq.>< addLetEnv xs (localEnv Seq.>< Seq.singleton(name, counter)) (counter+1)


-- translateLet EmptyExpr localEnv instructions = instructions ++ [SlideLet (length localEnv)]

-- translateLet [] expr localEnv instructions = instructions ++ translateExpr expr localEnv

-- translateLetExpr [] localEnv instructions globalExpr = translateLet [] EmptyExpr localEnv (instructions ++ translateExpr globalExpr localEnv) 
-- translateLetExpr ((LocDef name expr):xs) localEnv instructions globalExpr =  translateLetExpr xs (incrementPos localEnv) (translateExpr expr localEnv ++ [Alloc, Makeapp] ++ instructions) globalExpr


translatePlus (PlusX expr EmptyExpr) env = translateExpr expr env ++ push "+"
translatePlus (PlusX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (PlusX expr1 EmptyExpr) (increment env)
        
translateMinus (MinusX expr EmptyExpr) env = translateExpr expr env ++ push"-"
translateMinus (MinusX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (MinusX expr1 EmptyExpr) (increment env)

translateMult (MultX expr EmptyExpr) env = translateExpr expr env ++ push "*"
translateMult (MultX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (MultX expr1 EmptyExpr) (increment env)

translateDiv (DivX expr EmptyExpr) env = translateExpr expr env ++ push "/"
translateDiv (DivX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (DivX expr1 EmptyExpr) (increment env)

translateOr (OrX expr EmptyExpr) env = translateExpr expr env ++ push "|"
translateOr (OrX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (OrX expr EmptyExpr) (increment env)

translateAnd (AndX expr EmptyExpr) env = translateExpr expr env ++ push "&"
translateAnd (AndX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (AndX expr EmptyExpr) (increment env)

translateSmaller (SmallerX expr EmptyExpr) env = translateExpr expr env ++ push "<"
translateSmaller (SmallerX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (SmallerX expr EmptyExpr) (increment env)

translateEquals (EqualsX expr EmptyExpr) env = translateExpr expr env ++ push "=="
translateEquals (EqualsX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (EqualsX expr EmptyExpr) (increment env)

translateIf (IfX expr1 EmptyExpr EmptyExpr) env = translateExpr expr1 env ++ push "if"
translateIf (IfX expr1 expr2 EmptyExpr) env = makeapp $ translateExpr expr2 env ++ translateExpr (IfX expr1 EmptyExpr EmptyExpr) (increment env)
translateIf (IfX expr1 expr2 expr3) env = makeapp $ translateExpr expr3 env ++ translateExpr (IfX expr1 expr2 EmptyExpr) (increment env)

--------------------Local Environments--------------------
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
testProg2 = [FuncDef "f" ["a", "b"] (PlusX (VarX "a") (VarX "b"))]
testProg8 = [VarDef "a" (IfX (BoolX True) (IntX 1) (IntX 2))] ++ testProg2
testProg = [VarDef "a" (BoolX True), VarDef "b" (IntX 2)]
testProg3 = [VarDef "a" (IntX 1) ,VarDef "b" (PlusX (VarX "a") (IntX 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (AndX (BoolX True) (BoolX False))]
testProg5 = [VarDef "x" ( LetX [LocDef "a" (IntX 8), LocDef "b" (IntX 8)] (PlusX (VarX "a") (VarX "b")))]
