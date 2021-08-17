module DataStructures where

newtype InvalidSyntaxError = InvalidSyntaxError String
  deriving (Show)

data Prog = Prog Def RestProg | EmptyProg
  deriving (Show) 

data RestProg = Deps | RProg Prog
  deriving (Show)

data Def = Def Var Args Expr
  deriving (Show)

data Args = Aeps | Arg Var Args
  deriving (Show)

data LocDefs = LocDefs LocDef RestLocDefs
  deriving (Show)

data RestLocDefs = LDeps | RLocDefs LocDefs
  deriving (Show)

data LocDef = LocDef Var Expr
  deriving (Show)
  
data Expr
  = LetIn LocDefs Expr
  | IfThenElse Expr Expr Expr
  | Expr Expr1
  | EmptyExpr
  deriving (Show)

data Expr1 = Expr1 Expr2 RestExpr1
  deriving (Show)

data RestExpr1 = RE1eps | OR Expr2
  deriving (Show)

data Expr2 = Expr2 Expr3 RestExpr2
  deriving (Show)

data RestExpr2 = RE2eps | AND Expr3
  deriving (Show)

data Expr3 = Expr3 Expr4 RestExpr3 
  deriving (Show)

data RestExpr3 = RE3eps | CompEq' Expr4 | CompSmaller' Expr4
  deriving (Show)

data Expr4 = Expr4 Expr5 RestExpr4
  deriving (Show)

data RestExpr4 = RE4eps | PLUS Expr5 | MINUS Expr5
  deriving (Show)

data Expr5 = PosExpr5 Expr6 | NegExpr5 Expr6 
  deriving (Show)

data Expr6 = Expr6 Expr7 RestExpr6 
  deriving (Show)

data RestExpr6 = RE6eps | MULT Expr7 | DIV Expr7
  deriving (Show)

data Expr7 = Expr7 AtomicExpr RestExpr7 
  deriving (Show)

data RestExpr7 = RE7eps | App Expr7
  deriving (Show)

data AtomicExpr = AtomExpr AtomExpr | Parenthesised Expr 
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
