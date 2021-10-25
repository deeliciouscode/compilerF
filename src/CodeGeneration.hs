{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CodeGeneration where
import DataStructures
import Instructions

-------------------------- Main Functions --------------------------
generate :: Ast -> (GlobalEnvironment, Code)
generate ast = (generateDefList ast, generateCode ast)

generateDefList :: Ast -> GlobalEnvironment
generateDefList ast = setIndices 4 $ astToDefList ast ++ initDef

generateCode :: Ast -> Code
generateCode ast =  concatInstructions $ concatMap translateTree ast

concatInstructions :: [Instruction] -> [Instruction]
concatInstructions code = mHead ++ code ++ mTail

-------------------------- Global Environment --------------------------

createDefCell :: String -> Int -> [Instruction] -> DefCell
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
translateTree :: SubTree -> [Instruction]
translateTree subtree  =
    case subtree of
        VarDef name expr -> translateVar name expr (createLocalEnv [] 0)
        FuncDef name args expr -> translateFunc name args expr (createLocalEnv args 0)

translateVar :: String -> Expr -> LocalEnvironment -> [Instruction]
translateVar name expr locEnv = translExpr expr locEnv ++ varTail

translateFunc :: String -> Args -> Expr -> LocalEnvironment -> [Instruction]
translateFunc name args expr localEnv = translExpr expr localEnv ++ functionTail (length args)

translAndInc :: Expr -> LocalEnvironment -> [Instruction]
translAndInc expr env = translExpr expr (increment env)

translExpr :: Expr -> LocalEnvironment -> [Instruction]
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
        (NotX expr) -> translExpr expr locEnv ++ push "not"
        (NegX expr) -> translExpr expr locEnv ++ push "negate"
        e@(IfX expr1 expr2 expr3) -> translateIf e locEnv
        (LetX locDefs expr) -> translLocDefs locDefs locEnv (-1) ++ translLetExpr expr locDefs locEnv (-1)
        e@(AppX expr1 expr2) -> translAppExpr e locEnv

translLocDefs :: [LocDef] -> LocalEnvironment -> Int -> [Instruction]
translLocDefs [] _ _ = []
translLocDefs locdefs@((LocDef name expr):xs) localEnv i = translExpr expr localEnv ++ allocMake ++ translLocDefs xs localEnv' i
                    where
                        localEnv' = addLetEnv [head locdefs] (increment localEnv) i

translLetExpr :: Expr -> [LocDef] -> [(Name, Int)] -> Int -> [Instruction]
translLetExpr expr locDefs localEnv i = translExpr expr localEnv' ++ slideLet (length locDefs)
                    where
                         localEnv' = addLetEnv (reverse locDefs) (incrementN localEnv (length locDefs)) i

translatePlus :: Expr -> LocalEnvironment -> [Instruction]
translatePlus (PlusX expr EmptyExpr) env = translExpr expr env ++ push "+"
translatePlus (PlusX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (PlusX expr1 EmptyExpr) env

translateMinus :: Expr -> LocalEnvironment -> [Instruction]
translateMinus (MinusX expr EmptyExpr) env = translExpr expr env ++ push"-"
translateMinus (MinusX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (MinusX expr1 EmptyExpr) env

translateMult :: Expr -> LocalEnvironment -> [Instruction]
translateMult (MultX expr EmptyExpr) env = translExpr expr env ++ push "*"
translateMult (MultX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (MultX expr1 EmptyExpr) env

translateDiv :: Expr -> LocalEnvironment -> [Instruction]
translateDiv (DivX expr EmptyExpr) env = translExpr expr env ++ push "/"
translateDiv (DivX expr1 expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (DivX expr1 EmptyExpr) env

translateOr :: Expr -> LocalEnvironment -> [Instruction]
translateOr (OrX expr EmptyExpr) env = translExpr expr env ++ push "|"
translateOr (OrX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (OrX expr EmptyExpr) env

translateAnd :: Expr -> LocalEnvironment -> [Instruction]
translateAnd (AndX expr EmptyExpr) env = translExpr expr env ++ push "&"
translateAnd (AndX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (AndX expr EmptyExpr) env

translateSmaller :: Expr -> LocalEnvironment -> [Instruction]
translateSmaller (SmallerX expr EmptyExpr) env = translExpr expr env ++ push "<"
translateSmaller (SmallerX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (SmallerX expr EmptyExpr)  env

translateEquals :: Expr -> LocalEnvironment -> [Instruction]
translateEquals (EqualsX expr EmptyExpr) env = translExpr expr env ++ push "=="
translateEquals (EqualsX expr expr2) env = makeapp $ translExpr expr2 env ++ translAndInc (EqualsX expr EmptyExpr) env

translateIf :: Expr -> LocalEnvironment -> [Instruction]
translateIf (IfX expr1 EmptyExpr EmptyExpr) env = translExpr expr1 env ++ push "if"
translateIf (IfX expr1 expr2 EmptyExpr) env = makeapp $ translExpr expr2 env ++ translAndInc (IfX expr1 EmptyExpr EmptyExpr) env
translateIf (IfX expr1 expr2 expr3) env = makeapp $ translExpr expr3 env ++ translAndInc (IfX expr1 expr2 EmptyExpr) env

translAppExpr :: Expr -> LocalEnvironment -> [Instruction]
translAppExpr (AppX expr1 expr2) locEnv = makeapp $ translExpr expr2 locEnv ++ translAndInc expr1 locEnv

--------------------Local Environments--------------------
createLocalEnv :: Num t => [a] -> t -> [(a, t)]
createLocalEnv (x:xs) i@counter = (x,i+1) : createLocalEnv xs (i+1)
createLocalEnv [] _ = []

addLetEnv :: Num t => [LocDef] -> [(Name, t)] -> t -> [(Name, t)]
addLetEnv ((LocDef name expr):xs) localEnv i@counter =  addLetEnv xs ((name, i) : localEnv) (i+1)
addLetEnv [] localEnv counter = localEnv


updateLocalVarValue :: Num t => [LocDef] -> [(Name, t)] -> t -> [(Name, t)]
updateLocalVarValue localDef localEnv i@counter = if length localDef > 1 then addLetEnv localDef localEnv i else localEnv

initLocEnv :: [(a, Int)]
initLocEnv = createLocalEnv [] 0

isInEnv :: Eq t => t -> [(t, b)] -> Bool
isInEnv a ((x,y):xs) = a == x || isInEnv a xs
isInEnv a [] = False

getPos :: (Eq t, Num p) => t -> [(t, p)] -> p
getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 2

increment :: [(a, Int)] -> [(a, Int)]
increment = map(\(x,y) -> (x,y+1))

incrementN locEnv n = map(\(x,y) -> (x,y+n)) locEnv

testprog1 = [FuncDef "main" [] (AppX (VarX "f") (IntX 2)), FuncDef "f" ["a"] (LetX [(LocDef "b" (IntX 3)), (LocDef "c" (IntX 5))] (PlusX (VarX "a") (VarX "b")))]

testProg2 = [VarDef "f" (LetX [LocDef "a" (IntX 1), LocDef "b" (VarX "a")] (PlusX (VarX "a") (VarX "b"))) ]
-------------------



wichtiger code

wichtiger code 2