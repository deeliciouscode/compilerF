import Data.Maybe
import Data.Map

import Parser
-- import Relude
import Data.Text

import Helpers
import Lexer
import DataStructures

{--
Data Emulator = Memory Register Instructions

Data Memory = Code Stack Global Heap

Data Stack = Stack

Data Register = InstructionReg TopReg ProgramCounter

data FunctionReg = FunctionReg [(String, CodeIndex)]

data Code = Code [Instructions]
data HeapAddress = HeapAdress APP | HeapAdress GlobalAddress |
HeapAdress VAL

data GlobalAddress = DEF String Int CodeAddress

data Global = Global [GlobalAddress]

data Store = Store Global Code

<<<<<<< HEAD
data InstructionRegister = Int
data TopRegister = Int
data ProgramCounter = Int
data CompiledCode a = CompiledCode [Instructions a]
--}

=======

data InstructionRegister = Int
data TopRegister = Int
data ProgramCounter = Int
--}


--data CompiledCode a = CompiledCode [Instructions a]

>>>>>>> added function to unpack expressions; shortened expression names
data Instructions = 
    Pushfun String | 
    Pushval Expr | 
    Reset | 
<<<<<<< HEAD
    Pushparam Int | 
=======
    Pushparam Expr | 
>>>>>>> added function to unpack expressions; shortened expression names
    Makeapp | 
    Slide Expr | 
    Return | 
    Halt | 
    Call | 
    Unwind | 
    Operator Expr | 
    Alloc |
    SlideLet Expr

    deriving (Show)

translateProg :: [Def] -> [Instructions]
translateProg (x:xs) = translateDef x [] ++ translateProg xs
translateProg [] = []
-- translateProg xs = Prelude.foldr (\ x -> (++) (translateDef x [])) [] xs

<<<<<<< HEAD
-- TODO Implement FuncDef;
=======
>>>>>>> added function to unpack expressions; shortened expression names
translateDef :: Def -> [Instructions] -> [Instructions]
translateDef def list = 
    case def of 
        VarDef name expr -> translateVar name expr list
        --FuncDef name args expr -> translateFunc name args expr list

translateVar :: String -> Expr -> [Instructions] -> [Instructions]
translateVar name expr list = Prelude.reverse (Pushfun name : translateExpr expr ++ list)

<<<<<<< HEAD
-- TODO Implement cases Var & Let;
translateExpr :: Expr -> [Instructions]
translateExpr expr = 
    case expr of
        Var a -> [Pushfun a]
        -- Expr a -> 
        -- Let (LocDefs x:xs) a ->
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushval (Bool a)]
        (Or expr1 expr2) -> [Makeapp] ++ [Makeapp]++ [Pushfun "Or"] ++ translateExpr expr1 ++ translateExpr expr2
        (Equals expr1 expr2) -> [Makeapp] ++ [Makeapp]++ [Pushfun "Equals"] ++ translateExpr expr1 ++ translateExpr expr2
        (Smaller expr1 expr2) -> [Makeapp] ++ [Makeapp]++ [Pushfun "Equals"] ++ translateExpr expr1 ++ translateExpr expr2
        (Plus expr1 expr2) -> [Makeapp] ++ [Makeapp]++ [Pushfun "Plus"] ++ translateExpr expr1 ++ translateExpr expr2
        (Minus expr1 expr2) -> [Makeapp] ++ [Makeapp]++ [Pushfun "Minus"] ++ translateExpr expr1 ++ translateExpr expr2
        (Neg a) -> [Pushfun "Neg"] ++ translateExpr a
        (Pos a) -> [Pushfun "Pos"] ++ translateExpr a
        (Mult expr1 expr2) -> [Makeapp] ++ [Makeapp]++  [Pushfun "Mult"] ++ translateExpr expr1 ++ translateExpr expr2
        (Div expr1 expr2) -> [Makeapp] ++ [Makeapp]++ [Pushfun "Div"] ++ translateExpr expr1 ++ translateExpr expr2
        (App expr1 expr2) -> [Makeapp] ++ [Makeapp]++ [Pushfun "App"] ++ translateExpr expr1 ++ translateExpr expr2
        (If expr1 expr2 expr3) -> [Makeapp] ++ [Makeapp]++ [Makeapp]++ [Pushfun "If"] ++ translateExpr expr1 ++ translateExpr expr2
        EmptyExpr -> []


translateLocDefs ((LocDef name expr):xs) = translateLocDefs xs ++ translateExpr expr ++ Pushfun name : []

--giveIndex x (FuncDef name args express) = (x !! args) + 1

-- translateFuncDev name args expr list = Pushfun name : translateArgs (createLocalEnv args []) ++ translateExpr expr ++ list
translateFuncDev name args expr list = Pushfun name : translateArgs args ++ list


translateArgs args = translateLocalEnv (createLocalEnv args [])

-- createLocalEnv :: Args -> [a] -> [LocalEnvironment]
createLocalEnv (x:xs) list =  (x, Prelude.length (x:xs)) : createLocalEnv xs list
createLocalEnv [] _  = []

-- translateLocalEnv :: [LocalEnvironment] -> [Instructions]
translateLocalEnv (x:xs) = Pushparam (snd x) : (translateLocalEnv xs) 

-- newtype LocalEnvironment = LocalEnv (Arg, Int)






--- Test Cases ---

testProg = [VarDef "a" (Bool True), VarDef "b" (Int 2)]
testProg2 = [VarDef "a" (Or (Bool True) (Bool False))]
testProg3 = [VarDef "a" (Pos (Int 1)),VarDef "b" (Plus (Pos (Var "a")) (Pos (Int 2)))]



=======
translateExpr :: Expr -> [Instructions]
translateExpr expr = 
    case expr of
        -- Var a ->
        -- Expr a -> 
        -- LetIn (LocDefs x:xs) a ->
        Int a -> [Pushval (Int a)]
        Bool a -> [Pushval (Bool a)]
        (Or expr1 expr2) -> [Pushfun "Or"] ++ translateExpr expr1 ++ translateExpr expr2
        (And expr1 expr2) -> [Pushfun "And"] ++ translateExpr expr1 ++ translateExpr expr2
        (Equals expr1 expr2) -> [Pushfun "Equals"] ++ translateExpr expr1 ++ translateExpr expr2
        (Smaller expr1 expr2) -> [Pushfun "Equals"] ++ translateExpr expr1 ++ translateExpr expr2
        (Plus expr1 expr2) -> [Pushfun "Plus"] ++ translateExpr expr1 ++ translateExpr expr2
        (Minus expr1 expr2) -> [Pushfun "Minus"] ++ translateExpr expr1 ++ translateExpr expr2
        (Neg a) -> [Pushfun "Neg"] ++ translateExpr a
        (Pos a) -> [Pushfun "Pos"] ++ translateExpr a
        (Mult expr1 expr2) -> [Pushfun "Mult"] ++ translateExpr expr1 ++ translateExpr expr2
        (Div expr1 expr2) -> [Pushfun "Div"] ++ translateExpr expr1 ++ translateExpr expr2
        (App expr1 expr2) -> [Pushfun "App"] ++ translateExpr expr1 ++ translateExpr expr2
        (If expr1 expr2 expr3) -> [Pushfun "If"] ++ translateExpr expr1 ++ translateExpr expr2
        EmptyExpr -> []

testProg = [VarDef "a" (DataStructures.Bool True), VarDef "b" (DataStructures.Int 2)]
testProg2 = [VarDef "a" (DataStructures.Or (DataStructures.Bool True) (DataStructures.Bool False))]
testProg3 = [VarDef "a" (Pos (Int 1)),VarDef "b" (Plus (Pos (Var "a")) (Pos (Int 2)))]
>>>>>>> added function to unpack expressions; shortened expression names
