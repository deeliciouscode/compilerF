module DataStructures where

-- replicate syntax on page 62

data Program = Prog Definition MoreDefinitions
data MoreDefinitions = DEps | Program

data Definition = Def Var MultiVars Expr
data MultiVars = VEps | More Var MultiVars

data LocalDefs = LocDef LocalDef MoreLocalDefs 
data MoreLocalDefs = LDEps | LocalDefs LocalDefs

data LocalDef = LocalDef Variable Expr

data Var = Var

data Expr
  = Let LocalDef Expr
  | If Expr Expr Expr
  | BinExpr Expr BinaryOp Expr
  | UniExpr UniOp Expr
  | Twice Expr Expr
  | Parenthesised Expr
  | AtomExpr

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
  = T_VAR Variable
  | T_INT Int
  | T_BOOL Bool
  deriving (Show, Eq)

newtype Variable = Name String
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
  deriving (Show, Eq)

data Context = CtxDef | CtxLocalDef | CtxOther
  deriving (Show)


-- data SyntaxGraph

-- EBNF Meta Grammar

-- data Grammar = Prod Produktion |

-- Make LL(1) Grammar

-- ORIGINAL:
-- Programm             ::= Definition ";" { Definition ";"} .
-- Definition           ::= Variable {Variable} "=" Ausdruck .
-- Lokaldefinitionen    ::= Lokaldefinition { ";" Lokaldefinition } .
-- Lokaldefinition      ::= Variable "=" Ausdruck .
-- Ausdruck             ::= "let" Lokaldefinitionen "in" Ausdruck
--                        | "if" Ausdruck "then" Ausdruck "else" Ausdruck
--                        | Ausdruck BinärOp Ausdruck
--                        | UnärOp Ausdruck
--                        | Ausdruck Ausdruck
--                        | "(" Ausdruck ")"
--                        | AtomarerAusdruck .
-- BinärOp              ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
-- UnärOp               ::= "not" | "−" .
-- AtomarerAusdruck     ::= Variable | Zahl | Wahrheitswert .
-- Variable             ::= Name .

-- abstract form:
-- P ::= Ds | DsD
-- D ::= VeA | VV
-- A ::= lLiA | iAtAeA | AbA |

-- LL(1):
-- Programm             ::= Definition ";" { Definition ";"} .
-- Definition           ::= Variable {Variable} "=" Ausdruck .
-- Lokaldefinitionen    ::= Lokaldefinition { ";" Lokaldefinition } .
-- Lokaldefinition      ::= Variable "=" Ausdruck .
-- Ausdruck             ::= "let" Lokaldefinitionen "in" Ausdruck
--                        | "if" Ausdruck "then" Ausdruck "else" Ausdruck
--                        | Ausdruck Restausdruck
--                        | UnärOp Ausdruck
--                        | "(" Ausdruck ")"
--                        | AtomarerAusdruck .
-- Restausdruck         ::= Ausdruck | BinärOp Ausdruck
-- BinärOp              ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
-- UnärOp               ::= "not" | "−" .
-- AtomarerAusdruck     ::= Variable | Zahl | Wahrheitswert .
-- Variable             ::= Name .

-- LL(1) Wrong:
-- Programm             ::= Definition ";" { Definition ";"} .
-- Definition           ::= Variable {Variable} "=" Ausdruck .
-- Lokaldefinitionen    ::= Lokaldefinition { ";" Lokaldefinition } .
-- Lokaldefinition      ::= Variable "=" Ausdruck .
-- Ausdruck             ::= "let" Lokaldefinitionen "in" Ausdruck
--                        | "if" Ausdruck "then" Ausdruck "else" Ausdruck
--                        | Ausdruck Restausdruck
--                        | UnärOp Ausdruck
--                        | "(" Ausdruck ")"
--                        | AtomarerAusdruck .
-- Restausdruck         ::= Ausdruck | BinärOp Ausdruck
-- BinärOp              ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
-- UnärOp               ::= "not" | "−" .
-- AtomarerAusdruck     ::= Variable | Zahl | Wahrheitswert .
-- Variable             ::= Name .