module SemanticTree where
import Data.Maybe
import Data.Map

import Parser
-- import Relude
import Data.Text

import Helpers
import Lexer
import DataStructures

{--
data Emulator = Memory Register
Data Memory = Code Stack Global Heap

data Code = Code [Instructions]
data Stack = Stack
data Global = Global [GlobalAddress]
data Heap = Heap

data Registers a = Instructions a | Address a
data Address = Top Int | ProgramCounter Int

data FunctionReg = FunctionReg [(String, CodeIndex)]

data HeapAddress = HeapAdress APP | HeapAdress GlobalAddress |
HeapAdress VAL

data GlobalAddress = DEF String Int CodeAddress


data Store = Store Global Code

data CompiledCode a = CompiledCode [Instructions a]
--}


data Instructions =
    Pushfun String |
    Pushval Expr |
    Reset |
    Pushparam Int |
    Makeapp |
    Slide Int |
    Return |
    Halt |
    Call |
    Unwind |
    Operator Expr |
    Alloc |
    SlideLet Expr |
    Update Int
    deriving (Show)

translateProg :: [Def] -> [Instructions]
translateProg xs = Prelude.foldr (\ x -> (++) (translateDef x [])) [] xs

-- TODO Implement FuncDef;
translateDef :: Def -> [Instructions] -> [Instructions]
translateDef def list =
    case def of
        VarDef name expr -> translateVar name expr list
        FuncDef name args expr -> translateFunc name args expr list -- ++ [Update, Slide 1, Unwind, Call, Return]

translateVar :: String -> Expr -> [Instructions] -> [Instructions]
translateVar name expr list = Pushfun name : translateExpr expr ++ list

translateFunc :: String -> [Arg] -> Expr -> [Instructions] -> [Instructions]
translateFunc name args expr list = Pushfun name : translateArgs args ++ translateExpr expr ++ list

-- TODO Implement cases Var & Let;
translateExpr :: Expr -> [Instructions]
translateExpr expr =
    case expr of
        Var a -> [Pushfun a]
        -- Let ((LocDef name (expr):xs) (Expr a) -> translateLocDefs locDefs : translateExpr a
        Let a b -> Prelude.reverse(translateLocDefs a) ++ translateExpr b
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushval (Bool a)]
        (Or expr1 expr2) -> makeApp2 ++ [Pushfun "Or"] ++ translateExpr expr1 ++ translateExpr expr2
        (And expr1 expr2) -> translateExpr expr2 ++ translateExpr expr1 ++ [Pushfun "And"] ++ makeApp2
        (Not expr) -> [Makeapp] ++ [Pushfun "Not"] ++ translateExpr expr
        (Equals expr1 expr2) -> makeApp2 ++ [Pushfun "Equals"] ++ translateExpr expr1 ++ translateExpr expr2
        (Smaller expr1 expr2) -> makeApp2 ++ [Pushfun "Equals"] ++ translateExpr expr1 ++ translateExpr expr2
        (Plus expr1 expr2) -> translateExpr expr2 ++ translateExpr expr1 ++ [Pushfun "Plus"] ++ makeApp2 
        (Minus expr1 expr2) -> makeApp2 ++ [Pushfun "Minus"] ++ translateExpr expr1 ++ translateExpr expr2
        (Neg a) -> [Pushfun "Neg"] ++ translateExpr a
        (Mult expr1 expr2) -> makeApp2 ++  [Pushfun "Mult"] ++ translateExpr expr1 ++ translateExpr expr2
        (Div expr1 expr2) -> makeApp2 ++ [Pushfun "Div"] ++ translateExpr expr1 ++ translateExpr expr2
        (App expr1 expr2) -> makeApp2 ++ [Pushfun "App"] ++ translateExpr expr1 ++ translateExpr expr2
        (If expr1 expr2 expr3) -> makeApp3 ++ [Pushfun "If"] ++ translateExpr expr1 ++ translateExpr expr2
        EmptyExpr -> []


makeApp2 = [Makeapp, Makeapp]
makeApp3 = [Makeapp, Makeapp, Makeapp]

translateLocDefs :: [LocDef] -> [Instructions]
translateLocDefs ((LocDef name expr):xs) = translateLocDefs xs ++ translateExpr expr ++ Pushfun name : []
translateLocDefs [] = []


translateArgs :: [Arg] -> [Instructions]
translateArgs args = translateLocalEnv (createLocalEnv args [])

createLocalEnv (x:xs) list =  (x, Prelude.length (x:xs)) : createLocalEnv xs list
createLocalEnv [] _  = []

translateLocalEnv = Prelude.map (Pushparam . snd)

--- Test Cases ---

testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg2 = [VarDef "a" (Or (Bool True) (Bool False))]
testProg3 = [VarDef "a" (Int 1) ,VarDef "b" (Plus (Var "a") (Int 2))]

testProg4 = [FuncDef "func" ["a","b","c"] (And (Bool True) (Bool False))]

testProg5 = [VarDef "x" (Let [LocDef "a" (Int 8), LocDef "b" (Int 13)] (Plus (Var "a") (Var "b")))]

--- TODO functions Slide, Update, LetSlide, 




-- translateExpr (Or expr1 expr2) = translate2Expr (expr1 expr2) ++ Pushfun "Or"  ++ Makeapp : []

--         APP
--     APP     b
-- Plus    a


-- translate2Expr expr1 expr2 = translateExpr expr2 ++ translateExpr2 ++ Makeapp