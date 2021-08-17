module DataStructures where

newtype InvalidSyntaxError = InvalidSyntaxError String
  deriving (Show)

data Prog = Prog Def RestProg | EmptyProg
  deriving (Show) 

data RestProg = Deps | RProg Prog
  deriving (Show)

data Def = Def Var Args Expr | EmptyDef
  deriving (Show)

data Args = Aeps | RVars Var Args
  deriving (Show)

data LocDefs = LocDefs LocDef RestLocDefs | EmptyLocDefs
  deriving (Show)

data RestLocDefs = LDeps | RLocDefs LocDefs
  deriving (Show)

data LocDef = LocDef Var Expr  | EmptyLocDef
  deriving (Show)
  
data Expr
  = LetIn LocDefs Expr
  | IfThenElse Expr Expr Expr
  | Expr Expr1
  | EmptyExpr
  deriving (Show)

data Expr1 = Expr1 Expr2 RestExpr1 | EmptyExpr1
  deriving (Show)

data RestExpr1 = RE1eps | OR Expr2
  deriving (Show)

data Expr2 = Expr2 Expr3 RestExpr2 | EmptyExpr2
  deriving (Show)

data RestExpr2 = RE2eps | AND Expr3
  deriving (Show)

data Expr3 = Expr3 Expr4 RestExpr3 | EmptyExpr3
  deriving (Show)

data RestExpr3 = RE3eps | CompEq' Expr4 | CompSmaller' Expr4
  deriving (Show)

data Expr4 = Expr4 Expr5 RestExpr4 | EmptyExpr4
  deriving (Show)

data RestExpr4 = RE4eps | PLUS Expr5 | MINUS Expr5
  deriving (Show)

data Expr5 = PosExpr5 Expr6 | NegExpr5 Expr6 | EmptyExpr5
  deriving (Show)

data Expr6 = Expr6 Expr7 RestExpr6 | EmptyExpr6
  deriving (Show)

data RestExpr6 = RE6eps | MULT Expr7 | DIV Expr7
  deriving (Show)

data Expr7 = Expr7 AtomicExpr RestExpr7 | EmptyExpr7
  deriving (Show)

data RestExpr7 = RE7eps | App Expr7
  deriving (Show)

data AtomicExpr = AtomExpr AtomExpr | Parenthesised Expr  | EmptyAtomicExpr
  deriving (Show)

newtype Var = Name String
  deriving (Show, Eq)

--------------------------------------------------------

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

data AtomExpr
  = T_VAR Var
  | T_INT Int
  | T_BOOL Bool
  deriving (Show, Eq)

data Token
  = TAtomExpr AtomExpr
  | TBinOp BinaryOp
  | TUniOp UniOp
  | T_MAIN
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
