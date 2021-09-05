module DataStructures where

-- Aliases
type NumArgs    = Int
type CodeAdr    = Int
type HeapAdr    = Int
data Value      = Bool Bool | Int Int
            deriving (Show, Eq)

-- Stack & Heap Structures
data StackType  = C Int
                | H Int
            deriving (Show, Eq)

data HeapType   = DEF String NumArgs CodeAdr
                | IND HeapAdr
                | APP' HeapAdr HeapAdr
                | VAL Value
                | UNINIZIALIZED
            deriving (Show, Eq)

-- Storages 
-- type Code       = [Instructions] -- Output of Code Generation / defined below
type Stack      = [StackType]
-- type Global     = [(String, (NumArgs, CodeAdr))] -- Maybe have function that translates Operators to strings
type Heap       = [HeapType]

-- Registers
type I          = Instructions
type T          = Int
type P          = Int

-- Results
data Result     = RBool Bool
                | RInt Int
                | Placeholder
                | Debug String
                | RuntimeError String
            deriving (Show, Eq)

--------------------------------------------------------

data Instructions 
  = Pushfun String 
  | Pushval Expr 
  | Reset 
  | Pushparam Int 
  | Makeapp 
  | Slide Int 
  | Return 
  | Halt 
  | Call 
  | Unwind 
  | Operator OpInstrConstr
  | Alloc 
  | SlideLet Int 
  | Update Int
  | EmptyInstruction
  deriving (Show, Eq)  

data OpInstrConstr  
  = Plus 
  | Minus 
  | Times 
  | DividedBy 
  | Equals 
  | LessThan 
  | And
  | Or 
  | Not
  | Negate 
  | If
  deriving (Show, Eq)  

--------------------------------------------------------

newtype InvalidSyntaxError = InvalidSyntaxError String
  deriving (Show)

type Arg = String

type Name = String

type Ast = [SubTree]

type Args = [Arg]

type LocDefs = [LocDef]

type DefCell = (String, (Int,Int))

type GlobalEnvironment = [DefCell]

type Code = [Instructions]

type Output = (GlobalEnvironment, Code)

type LocalEnvironment = [(String, Int)]

data SubTree = FuncDef Name Args Expr | VarDef Name Expr | Deps
  deriving (Show)

data LocDef = LocDef Name Expr | LDeps
  deriving (Show, Eq) 

data Expr
  = LetX LocDefs Expr
  | IfX Expr Expr Expr
  | VarX String
  | IntX Int 
  | BoolX Bool
  | OrX Expr Expr
  | AndX Expr Expr
  | NotX Expr
  | EqualsX Expr Expr
  | SmallerX Expr Expr
  | PlusX Expr Expr
  | MinusX Expr Expr
  | NegX Expr
  | MultX Expr Expr
  | DivX Expr Expr
  | AppX Expr Expr
  | EmptyExpr
  deriving (Show, Eq)

data RestExpr1 = RE1eps | OR Expr
  deriving (Show)

data RestExpr2 = RE2eps | AND Expr
  deriving (Show)

data RestExpr3 = RE3eps | CompEq Expr | CompSmaller Expr
  deriving (Show)

data RestExpr4 = RE4eps | PLUS Expr | MINUS Expr
  deriving (Show)

data RestExpr6 = RE6eps | MULT Expr | DIV Expr
  deriving (Show)

data RestExpr7 = RE7eps | APP Expr
  deriving (Show)

newtype Var = Name String
  deriving (Show, Eq)

--------------------------------------------------------

data AtomExpr
  = T_VAR Var
  | T_INT Int
  | T_BOOL Bool
  deriving (Show, Eq)

data BinaryOp
  = BO_AND
  | BO_OR
  | BO_EQUAL
  | BO_SMALLER
  | BO_PLUS
  | BO_MINUS
  | BO_MUL
  | BO_DIV
  deriving (Show, Eq)

data UniOp
  = UO_NOT
  | UO_MINUS
  deriving (Show, Eq)

data Token
  = TAtomExpr AtomExpr
  | TBinOp BinaryOp
  | TUniOp UniOp
  | T_LET
  | T_IN
  | T_IF
  | T_THEN
  | T_ELSE
  | TLPAREN
  | TRPAREN
  | TNULL
  | TSEMICOL
  | TEQUAL
  | Error String
  deriving (Show, Eq)
