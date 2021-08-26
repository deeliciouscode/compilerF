module CodeGeneration where
import Data.Maybe
import Parser
import Helpers
import Lexer
import DataStructures
import Instructions

newtype LocalEnvironment = LocalEnvironment [(String,Int)] deriving(Eq)

translProg xs = foldr (\ x -> (++) (translDef x [])) []

translDef :: Def -> [Instructions] -> [Instructions]
translDef def list =
    case def of
        VarDef name expr -> translVar name expr list
        FuncDef name args expr -> translFunc name args expr list $ createLocalEnv args 1
        Deps -> list

translVar :: String -> Expr -> [Instructions] -> [Instructions]
translVar name expr list = translExpr expr [] ++ [Update 0, Slide 1, Unwind, Call, Return] ++ list 

translFunc name args expr list localEnv = translExpr expr localEnv ++ trainlingFunc (length args) ++ list

createLocalEnv (x:xs) i = (x,i) : createLocalEnv xs (i+1)
createLocalEnv [] _ = []

translExpr :: Expr -> [(String, Int)] -> [Instructions]
translExpr expr localEnv = 
    case expr of
        Var a -> if isInLocalEnv a localEnv then [Pushparam (getPos a localEnv)] else [Pushfun a]
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushfun (show a)]
        e@(Plus expxpr1 expr2) -> translPlus e localEnv
        e@(Minus expr expr2) -> translMinus e localEnv
        e@(Mult expr expr2) -> translMult e localEnv
        e@(Div expr1 expr2) -> translDiv e localEnv
        e@(Or expr expr2) -> translOr e localEnv 
        e@(And expr expr2) -> translAnd e localEnv
        e@(Smaller expr expr2) -> translSmaller e localEnv
        e@(Equals expr expr2) -> translEquals e localEnv
        (Not expr) -> translExpr expr localEnv ++ push "Not" 
        e@(Neg expr) -> translExpr expr localEnv ++ push "Negate"
        e@(If expr1 expr2 expr3) -> translIf e localEnv
        -- (Let locdefs expr) -> translLet locdefs expr (createLetEnv locDefs localEnv)
       
-- translLet (x:xs) expr localLetEnv = 

<<<<<<< HEAD
--translateLet (x:xs) expr localLetEnv = 
=======
translPlus (Plus expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "+"
translPlus e@(Plus expr1 expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 
>>>>>>> 79fe7d46f68a7a267ee8fbeb9f1a351dd3b5e586

translMinus (Minus expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "-"
translMinus e@(Minus expr1 expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 

translMult (Mult expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "*"
translMult e@(Mult expr1 expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 

translDiv (Div expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "/"
translDiv e@(Div expr1 expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 

translOr (Or expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "|"
translOr e@(Or expr expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 

translAnd (And expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "&"
translAnd e@(And expr expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 

translSmaller (Smaller expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "<"
translSmaller e@(Smaller expr expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 

translEquals :: Expr -> [(String, Int)] -> [Instructions]
translEquals (Equals expr EmptyExpr) localEnv = translExpr expr localEnv ++ push "=="
translEquals e@(Equals expr expr2) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv) 

translIf (If expr EmptyExpr EmptyExpr) localEnv = translExpr expr localEnv ++ push "if"
translIf e@(If expr1 expr2 EmptyExpr) localEnv = makeapp $ translExpr expr2 localEnv ++ translExpr e (updatePos localEnv)  
translIf e@(If expr1 expr2 expr3) localEnv = makeapp $ translExpr expr3 localEnv ++ translExpr e (updatePos localEnv) 


---------------Helpers----------------------
updatePos :: [(a, Int)] -> [(a, Int)]
updatePos = map (\(x,y) -> (x,y+1))

isInLocalEnv a ((x,y):xs) = (a == x) || isInLocalEnv a xs
isInLocalEnv a [] = False

getPos a ((x,y):xs) = if a == x then y else getPos a xs
getPos a [] = 0

push :: String -> [Instructions]
push operator = [Pushfun operator, Makeapp]

makeapp = (++) [Makeapp]

trainlingFunc n = [Update (n), Slide (n + 1), Unwind, Call, Return]

<<<<<<< HEAD
=======
{--
translLocDefs :: [LocDef] -> [Instructions]
translLocDefs ((LocDef name expr):xs) = translLocDefs xs ++ translExpr expr ++ Pushfun name : []
translLocDefs [] = []

--}
--- Test Cases ---


testProg2 = [FuncDef "a" ["a", "b"] (Plus (Var "a") (Var "b"))]
testProg8 = VarDef "a" (If (Bool True) (Int 1) (Int 2)) : testProg2
testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]
testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]
testProg5 = [VarDef "x" (Let [LocDef "a" (Int 8), LocDef "b" (Int 13)] (Plus (Var "a") (Var "b")))]

>>>>>>> 79fe7d46f68a7a267ee8fbeb9f1a351dd3b5e586

