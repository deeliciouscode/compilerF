import Data.Maybe
import Data.Map

import Parser
-- import Relude
import Data.Text

import Helpers
import Lexer
import DataStructures

--genAttTree :: (Maybe a, [token]) -> Map T_VAR (AtomExpr, AtomExpr)
genAttTree x = unpackProg $ fromJust $ fst x
unpackProg (Prog def restprog) = unpackDef def

unpackDef (Def var args expr) = Data.Map.singleton var (unpackExpr expr)

unpackExpr (Expr expr1) = unpackExpr1 expr1
unpackExpr _ = AtomExpr (T_INT 1)

unpackExpr1 (Expr1 expr2 restExpr1) = unpackExpr2 expr2

unpackExpr2 (Expr2 expr3 restExpr2) = unpackExpr3 expr3

unpackExpr3 (Expr3 expr4 restExpr3) = unpackExpr4 expr4

unpackExpr4 (Expr4 expr5 restExpr4) = unpackExpr5 expr5

unpackExpr5 (PosExpr5 expr6) = unpackExpr6 expr6
unpackExpr5 (NegExpr5 expr6) = unpackExpr6 expr6

unpackExpr6 (Expr6 expr7 restExpr6) = unpackExpr7 expr7

unpackExpr7 (Expr7 atomicExpr restExpr7) = atomicExpr


    
    
    



