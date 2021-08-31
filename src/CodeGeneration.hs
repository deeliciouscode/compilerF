{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}
module CodeGeneration where

import Parser
import Helpers
import Lexer
import DataStructures
import Instructions
import qualified Data.Sequence as Seq


<<<<<<< HEAD
-------------------------- Translation--------------------------
translateProg (x:xs) = translateDef x [] ++ translateProg xs
translateProg [] = []

translateDef def locEnv =
    case def of
        VarDef name expr -> translateVar name expr (Seq.fromList $ (createLocalEnv [] 0)) 
        FuncDef name args expr -> translateFunc name args expr (Seq.fromList $ (createLocalEnv args 0))

translateVar name expr localEnv  = translateExpr expr localEnv ++ [Update 0, Slide 1, Unwind, Call, Return]

translateFunc name args expr localEnv = translateExpr expr localEnv ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return]

=======
-------------------------- Main Functions --------------------------
-- TODO: add preexisting def cells and adjust indexes accordingly

generateDefList :: Ast -> DefList
generateDefList ast = setIndices 0 $ (astToDefList ast) ++ initDef

generateCode :: Ast -> Code
generateCode (x:xs) =  (translateTree x []) ++ generateCode xs

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
generateDefCell (VarDef name expr) = createDefCell name 0 (length (translateVar name expr []))
generateDefCell (FuncDef name args expr) = createDefCell name (length args) (length (translateFunc name args expr [] ([], createLocalEnv args 1)))

setIndices :: Int -> DefList -> DefList
setIndices n ((a,(x,y)):[]) = [(a,(x,n))]
setIndices n ((a,(x,y)):xs) = (a,(x,n)) : setIndices (y+n) xs

astToDefList :: Ast -> DefList
astToDefList ast = map generateDefCell ast

-------------------------- Translation --------------------------

translateTree :: SubTree -> Code -> Code
translateTree subtree list  =
    case subtree of
        VarDef name expr -> translateVar name expr list
        FuncDef name args expr -> translateFunc name args expr list ([], createLocalEnv args 1)

translateVar :: String -> Expr -> Code -> Code
translateVar name expr list = translateExpr expr ([],[]) ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list 

translateFunc name args expr list env =  translateExpr expr env ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return] ++ list

-- translateVar name expr localEnv  = translateExpr expr localEnv ++ [Update 0, Slide 1, Unwind, Call, Return]

-- translateFunc name args expr localEnv = translateExpr expr localEnv ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return]
-------------------------- Translation--------------------------
translateProg (x:xs) = translateDef x [] ++ translateProg xs
translateProg [] = []

translateDef def locEnv =
    case def of
        VarDef name expr -> translateVar name expr (Seq.fromList $ (createLocalEnv [] 0)) 
        FuncDef name args expr -> translateFunc name args expr (Seq.fromList $ (createLocalEnv args 0))

>>>>>>> 877c65dee67e92d13bf072139cccd03b0c290ba5
translateExpr expr locEnv = 
    case expr of
        Var a -> if isInEnv a locEnv then [Pushparam (getPos a locEnv)] else [Pushfun a]
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushfun (show a)]
        e@(Plus expr1 expr2) -> translatePlus e locEnv
        e@(Minus expr expr2) -> translateMinus e locEnv
        e@(Mult expr expr2) -> translateMult e locEnv
<<<<<<< HEAD
        e@(Div expr1 expr2) -> translateDiv e locEnv
        e@(Or expr expr2) -> translateOr e locEnv 
        e@(And expr expr2) -> translateAnd e locEnv
        e@(Smaller expr expr2) -> translateSmaller e locEnv
        e@(Equals expr expr2) -> translateEquals e locEnv
        (Not expr) -> translateExpr expr locEnv ++ push "Not"
        (Neg expr) -> translateExpr expr locEnv ++ push "Negate"
        e@(If expr1 expr2 expr3) -> translateIf e locEnv
        (Let locdefs expr) -> translateLocalDefs locdefs locEnv (-1) ++ translateLetExpr expr locDefs locEnv
=======
        e@(DivX expr1 expr2) -> translateDiv e locEnv
        e@(OrX expr expr2) -> translateOr e locEnv 
        e@(AndX expr expr2) -> translateAnd e locEnv
        e@(SmallerX expr expr2) -> translateSmaller e locEnv
        e@(EqualsX expr expr2) -> translateEquals e locEnv
        (Not expr) -> translateExpr expr locEnv ++ push "Not"
        (Neg expr) -> translateExpr expr locEnv ++ push "Negate"
        e@(If expr1 expr2 expr3) -> translateIf e locEnv
        (LetX locdefs expr) -> translateLocalDefs locdefs locEnv (-1) ++ translateLetExpr expr locDefs locEnv
>>>>>>> 877c65dee67e92d13bf072139cccd03b0c290ba5


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
<<<<<<< HEAD
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
=======
translatePlus (Plus expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (PlusX expr1 EmptyExpr) (increment env)
        
translateMinus (Minus expr EmptyExpr) env = translateExpr expr env ++ push"-"
translateMinus (Minus expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (MinusX expr1 EmptyExpr) (increment env)

translateMult (MultX expr EmptyExpr) env = translateExpr expr env ++ push "*"
translateMult (MultX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (MultX expr1 EmptyExpr) (increment env)

translateDiv (DivX expr EmptyExpr) env = translateExpr expr env ++ push "/"
translateDiv (DivX expr1 expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (DivX expr1 EmptyExpr) (increment env)

translateOr (OrX expr EmptyExpr) env = translateExpr expr env ++ push "|"
translateOr (OrX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (OrX expr EmptyExpr) (increment env)

translateAnd (And expr EmptyExpr) env = translateExpr expr env ++ push "&"
translateAnd (And expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (AndX expr EmptyExpr) (increment env)

translateSmaller (SmallerX expr EmptyExpr) env = translateExpr expr env ++ push "<"
translateSmaller (SmallerX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (SmallerX expr EmptyExpr) (increment env)

translateEquals (EqualsX expr EmptyExpr) env = translateExpr expr env ++ push "=="
translateEquals (EqualsX expr expr2) env = makeapp $ translateExpr expr2 env ++ translateExpr (EqualsX expr EmptyExpr) (increment env)

translateIf (IfX expr1 EmptyExpr EmptyExpr) env = translateExpr expr1 env ++ push "if"
translateIf (IfX expr1 expr2 EmptyExpr) env = makeapp $ translateExpr expr2 env ++ translateExpr (IfX expr1 EmptyExpr EmptyExpr) (increment env)
translateIf (IfX expr1 expr2 expr3) env = makeapp $ translateExpr expr3 env ++ translateExpr (IfX expr1 expr2 EmptyExpr) (increment env)
>>>>>>> 877c65dee67e92d13bf072139cccd03b0c290ba5

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
testProg2 = [FuncDef "f" ["a", "b"] (Plus (Var "a") (Var "b"))]
testProg8 = [VarDef "a" (If (Bool True) (Int 1) (Int 2))] ++ testProg2
testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]
testProg5 = [VarDef "x" ( Let [LocDef "a" (Int 8), LocDef "b" (Int 8)] (Plus (Var "a") (Var "b")))]
