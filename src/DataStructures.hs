module DataStructures where

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

data AtomExpr = T_VAR 
              | T_INT Int 
              | T_BOOL Bool
        deriving (Show, Eq)

data Token = TAtomExpr AtomExpr 
           | TBinOp BinaryOp
           | TUniOp UniOp
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