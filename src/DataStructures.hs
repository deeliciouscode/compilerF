module DataStructures where

newtype InvalidSyntaxError = InvalidSyntaxError String
  deriving (Show)

type Arg = String
type Name = String

type Prog = [Def]

data Def = FuncDef Name Args Expr | VarDef Name Expr | Deps
  deriving (Show)

type Args = [Arg]

type LocDefs = [LocDef]

data LocDef = LocDef Name Expr | LDeps
  deriving (Show)
  
data Expr
  = LetIn LocDefs Expr
  | If Expr Expr Expr
  | Expr Expr1
  | Var String
  | Int Int 
  | Bool Bool
  | Or Expr Expr
  | And Expr Expr
  | Equals Expr Expr
  | SmallerThan Expr Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Neg Expr
  | Mult Expr Expr
  | Div Expr Expr
  | App Expr Expr
  | EmptyExpr
  deriving (Show)

data Expr1 = Expr1 Expr2 RestExpr1
 deriving (Show)

data RestExpr1 = RE1eps | OR Expr
  deriving (Show)

data Expr2 = Expr2 Expr3 RestExpr2
  deriving (Show)

data RestExpr2 = RE2eps | AND Expr
  deriving (Show)

data Expr3 = Expr3 Expr4 RestExpr3 
  deriving (Show)

data RestExpr3 = RE3eps | CompEq Expr | CompSmaller Expr
  deriving (Show)

data Expr4 = Expr4 Expr5 RestExpr4
  deriving (Show)

data RestExpr4 = RE4eps | PLUS Expr | MINUS Expr
  deriving (Show)

data Expr5 = PosExpr5 Expr | NegExpr5 Expr 
  deriving (Show)

data Expr6 = Expr6 Expr7 RestExpr6 
  deriving (Show)

data RestExpr6 = RE6eps | MULT Expr | DIV Expr
  deriving (Show)

data Expr7 = Expr7 AtomicExpr RestExpr7 
  deriving (Show)

data RestExpr7 = RE7eps | APP Expr
  deriving (Show)

newtype AtomicExpr = AtomExpr Expr
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
