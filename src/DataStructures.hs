module DataStructures where

-- replicate syntax on page 62

data Program = OneDef Definition 
             | MultiDefs Program Program

data Definition = Var String Expr | Vars [String] Expr -- not sure if second part makes sense

data LocalDefs = OneLocDef LocalDef | MultiLocDefs LocalDefs LocalDefs

data LocalDef = LocalDef Variable Expr

data Expr = Let LocalDef Expr
          | If Expr Expr Expr
          | BinExpr Expr BinaryOp Expr
          | UniExpr UniOp Expr
          | Twice Expr Expr
          | Parenthesised Expr
          | AtomExpr

data BinaryOp = BO_AND
              | BO_OR
              | BO_EQUAL
              | BO_UNEQUAL
              | BO_GREATER
              | BO_GREATER_EQUAL
              | BO_SMALLER
              | BO_SMALLER_EQUAL
              | BO_PLUS
              | BO_MINUS
              | BO_MUL
              | BO_DIV
        deriving (Show, Eq)

data UniOp = UO_NOT
           | UO_MINUS
        deriving (Show, Eq)

data AtomExpr = T_VAR Variable
              | T_INT Int 
              | T_BOOL Bool
        deriving (Show, Eq)

newtype Variable = Name String 
        deriving (Show, Eq)

data Token = TAtomExpr AtomExpr 
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
           | TCONTAINER [Token]
        deriving (Show, Eq)


-- Maybe replaced later
data SyntaxTree a = Leaf a | Node (SyntaxTree a) (SyntaxTree a)
        deriving Show