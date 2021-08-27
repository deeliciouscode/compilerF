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

<<<<<<< HEAD
translateProg xs = foldr (\ x -> (++) (translateDef x [])) [] xs
=======
translateProg xs = Prelude.foldr (\ x -> (++) (translateDef x [])) [] xs
>>>>>>> bed7801c3f756064a75ec6ab99512cda2767bd4a

translateDef :: Def -> [Instructions] -> [Instructions]
translateDef def list =
    case def of
        VarDef name expr -> translateVar name expr list
        FuncDef name args expr -> translateFunc name args expr list $ createLocalEnv args 1
        Deps -> list

translateVar :: String -> Expr -> [Instructions] -> [Instructions]
translateVar name expr list = translateExpr expr [] ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list 

<<<<<<< HEAD
translateFunc name args expr list localEnv = translateExpr expr localEnv ++ [Update (length args), Slide (length args + 1), Unwind, Call, Return] ++ list
=======
translateFunc name args expr list localEnv = translateExpr expr localEnv ++ [Update (Prelude.length args), Slide (Prelude.length args + 1), Unwind, Call, Return] ++ list
>>>>>>> bed7801c3f756064a75ec6ab99512cda2767bd4a

createLocalEnv (x:xs) counter = (x,counter) : createLocalEnv xs (counter+1)
createLocalEnv [] _ = []

<<<<<<< HEAD

=======
>>>>>>> bed7801c3f756064a75ec6ab99512cda2767bd4a
translateExpr :: Expr -> [(String, Int)] -> [Instructions]
translateExpr expr localEnv = 
    case expr of
        Var a -> if isInLocalEnv a localEnv then [Pushparam (getPos a localEnv)] else [Pushfun a]
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushfun (show a)]
        e@(Plus expr1 expr2) -> translatePlus e localEnv
        e@(Minus expr expr2) -> translateMinus e localEnv
        e@(Mult expr expr2) -> translateMult e localEnv
        e@(Div expr1 expr2) -> translateDiv e localEnv
        e@(Or expr expr2) -> translateOr e localEnv 
        e@(And expr expr2) -> translateAnd e localEnv
        e@(Smaller expr expr2) -> translateSmaller e localEnv
        e@(Equals expr expr2) -> translateEquals e localEnv
        (Not expr) -> translateExpr expr localEnv ++ push "Not"
        (Neg expr) -> translateExpr expr localEnv ++ push "Negate"
        e@(If expr1 expr2 expr3) -> translateIf e localEnv
<<<<<<< HEAD
        (Let locdefs expr) -> translateLet locdefs expr (createLetEnv locdefs localEnv 0)
        

-- createLetEnv ((LocDef name expr):xs) localEnv counter = (name,counter+1) : createLetEnv xs localEnv counter+1 : localEnv
-- createLetEnv [] _ _ = []

=======
        -- (Let locdefs expr) -> translateLet locdefs expr (createLetEnv locDefs localEnv)
        
>>>>>>> bed7801c3f756064a75ec6ab99512cda2767bd4a
-- translateLet (x:xs) expr localLetEnv = 

translatePlus (Plus expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push "+"
translatePlus (Plus expr1 expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (Plus expr1 EmptyExpr) (updatePos localEnv)
        
translateMinus (Minus expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push"-"
translateMinus (Minus expr1 expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (Minus expr1 EmptyExpr) (updatePos localEnv)

translateMult (Mult expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push "*"
translateMult (Mult expr1 expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (Mult expr1 EmptyExpr) (updatePos localEnv)

translateDiv (Div expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push "/"
translateDiv (Div expr1 expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (Div expr1 EmptyExpr) (updatePos localEnv)

translateOr (Or expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push "|"
translateOr (Or expr expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (Or expr EmptyExpr) (updatePos localEnv)

translateAnd (And expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push "&"
translateAnd (And expr expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (And expr EmptyExpr) (updatePos localEnv)

translateSmaller (Smaller expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push "<"
translateSmaller (Smaller expr expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (Smaller expr EmptyExpr) (updatePos localEnv)

translateEquals (Equals expr EmptyExpr) localEnv = translateExpr expr localEnv ++ push "=="
translateEquals (Equals expr expr2) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (Equals expr EmptyExpr) (updatePos localEnv)

translateIf (If expr1 EmptyExpr EmptyExpr) localEnv = translateExpr expr1 localEnv ++ push "if"
translateIf (If expr1 expr2 EmptyExpr) localEnv = makeapp $ translateExpr expr2 localEnv ++ translateExpr (If expr1 EmptyExpr EmptyExpr) (updatePos localEnv)
translateIf (If expr1 expr2 expr3) localEnv = makeapp $ translateExpr expr3 localEnv ++ translateExpr (If expr1 expr2 EmptyExpr) (updatePos localEnv)

--------------------Helpers-------------------------------
isInLocalEnv a ((x,y):xs) = (a == x) || isInLocalEnv a xs
isInLocalEnv a [] = False

getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 0
<<<<<<< HEAD
updatePos = map (\(x,y) -> (x,y+1))
=======
updatePos = Prelude.map (\(x,y) -> (x,y+1))
>>>>>>> bed7801c3f756064a75ec6ab99512cda2767bd4a

makeapp a = a ++ [Makeapp]
push op = [Pushfun op, Makeapp]




{--
translateLocDefs :: [LocDef] -> [Instructions]
translateLocDefs ((LocDef name expr):xs) = translateLocDefs xs ++ translateExpr expr ++ Pushfun name : []
translateLocDefs [] = []

--}
--- Test Cases ---
testProg2 = [FuncDef "a" ["a", "b"] (Plus (Var "a") (Var "b"))]
testProg8 = [VarDef "a" (If (Bool True) (Int 1) (Int 2))] ++ testProg2
testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]
testProg5 = [VarDef "x" (Let [LocDef "a" (Int 8), LocDef "b" (Int 13)] (Plus (Var "a") (Var "b")))]