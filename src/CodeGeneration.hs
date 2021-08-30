-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CodeGeneration where

import Parser
import Helpers
import Lexer
import DataStructures
import Instructions

generateDEF :: DefList
generateDEF = 
    [("if", (3,4))
    , ("<",(2,15))
    , ("==",(2,25))
    , ("/",(2,35))
    , ("*",(2,45))
    , ("-",(2,55))
    , ("+",(2,65))
    , ("|",(2,75))
    , ("&",(2,85))
    , ("not",(1,85))
    ]



---------------- Maybe later-------------------------------
-- genOutput [] output = output
-- genOutput (x:xs) (a,b) = do 
--     instructions <- translateTree x []
--     defList <- genDefList x (length instructions)
--     let outPut = genOutput xs (a++[instructions], defList) 
--     return outPut

-- genDefList :: SubTree -> Int -> DefList -> DefList
-- genDefList (FuncDef name args expr) offset defList = insertDef name (length args) offset defList
-- genDefList (VarDef name expr) offset defList = insertDef name 0 offset defList
---------------- end maybe-------------------------------
{--

insertDef :: String -> Int -> Int -> DefList -> DefList
insertDef name arity length [(op,(n,i))] =  [(name, (arity, 4))] ++ addn length [(op,(n, i))]

-- generateDefCellList :: Ast -> DefList -> DefList
-- generateDefCellList [] deflist = deflist
-- generateDefCellList (x:xs) deflist = translateTree' x deflist : generateDefCellList xs deflist


generateDefCellList :: Ast -> DefList -> DefList
generateDefCellList [] = []
generateDefCellList (x:xs) (prevName, (arity, l):y) = translateTree' x l : generateDefCellList xs 

--}

generateDefCellList :: Ast -> DefList
generateDefCellList [] = []
-- generateDefCellList (x:xs) = insertDefCell generateDEF

generateDefCell :: String -> Int -> Int -> DefCell
generateDefCell name arity length =  (name, (arity, length))


----------------------------
-- TODO: add preexisting def cells and adjust indexes accordingly


-- generate the deflist from the list abstrac syntax tree 
entry :: Ast -> DefList
entry ast = incInd 0 (map treeToDef ast)

-- treeToDef :: SubTree -> DefList
-- Generate one def cell from one subtree
treeToDef (VarDef name expr) = generateDefCell name 0 (length (translateVar name expr []))
treeToDef (FuncDef name args expr) = generateDefCell name (length args) (length (translateFunc name args expr [] ([], createLocalEnv args 1)))

-- incInd :: Int -> DefList -> 
-- After deflist has been constructed set the indexes to their respective value
incInd n ((a,(x,y)):[]) = [(a,(x,n))]
incInd n ((a,(x,y)):xs) = (a,(x,n)) : incInd (y+n) xs

insertDef :: String -> Int -> Int -> DefList -> DefList
insertDef name arity length [(op,(n,i))] =  [(name, (arity, 4))] ++ addn length [(op,(n, i))]
---------------------------

addn n = map (\(x,(y, z))->(x,(y, z+n)))

-------------------------- Translation--------------------------
-- translateProg xs def= foldr (\ x -> (++) (translateTree x def)) xs
translateProg (x:xs) def =  (translateTree x def) : translateProg xs def

translateTree subtree list  =
    case subtree of
        VarDef name expr -> translateVar name expr list
        FuncDef name args expr -> translateFunc name args expr list ([], createLocalEnv args 1)

translateVar :: String -> Expr -> Code -> Code
translateVar name expr list = translateExpr expr ([],[]) ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list 

translateFunc name args expr list env =  translateExpr expr env ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return] ++ list

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
testProg = [VarDef "a" (IntX 12), VarDef "b" (IntX 2)]
testProg3 = [VarDef "a" (IntX 1) ,VarDef "b" (PlusX (VarX "a") (IntX 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (AndX (BoolX True) (BoolX False))]
testProg5 = [VarDef "x" ( LetX [LocDef "a" (IntX 8), LocDef "b" (IntX 8)] (PlusX (VarX "a") (VarX "b")))]

testCode = [("a",(0,6)),("x",(2,10)),("b",(0,6)),("c",(0,6)),("a",(2,10))]
testCode2 = [("a",(2,10))]