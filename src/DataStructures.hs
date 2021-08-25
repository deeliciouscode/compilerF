module DataStructures where

newtype InvalidSyntaxError = InvalidSyntaxError String
  deriving (Show)

type Arg = String

type Name = String

type Prog = [Def]

type Args = [Arg]

type LocDefs = [LocDef]

data Def = FuncDef Name Args Expr | VarDef Name Expr | Deps
  deriving (Show)

data LocDef = LocDef Name Expr | LDeps
  deriving (Show)

data Instructions 
  = Pushfun String 
  |Pushval Expr 
  |Reset 
  |Pushparam Int 
  |Makeapp 
  |Slide Int 
  |Return 
  |Halt 
  |Call 
  |Unwind 
  |Operator Expr 
  |Alloc 
  |SlideLet Expr 
  |Update Int
  deriving (Show)  

  
data Expr
  = Let LocDefs Expr
  | If Expr Expr Expr
  | Var String
  | Int Int 
  | Bool Bool
  | Or Expr Expr
  | And Expr Expr
  | Not Expr
  | Equals Expr Expr
  | Smaller Expr Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Neg Expr
  | Mult Expr Expr
  | Div Expr Expr
  | App Expr Expr
  | EmptyExpr
  deriving (Show)

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
