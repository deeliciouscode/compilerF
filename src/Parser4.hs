-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser4 where
import Relude

-- import DataStructures
import Helpers

newtype InvalidSyntaxError = InvalidSyntaxError String
  deriving (Show)

data Prog = Prog Def RestProg

data RestProg = Deps | RProg Prog

data Def = Def Var RestVars Expr

data RestVars = Veps | RVars Var RestVars

data LocDefs = LocDefs LocDef RestLocDefs

data RestLocDefs = LDeps | RLocDefs LocDefs

data LocDef = LocDef Var Expr

data Expr
  = LetIn LocDefs Expr
  | IfElseThen Expr Expr Expr
  | Expr Expr1

data Expr1 = Expr1 Expr2 RestExpr1

data RestExpr1 = RE1eps | OR Expr1

data Expr2 = Expr2 Expr3 RestExpr2

data RestExpr2 = RE2eps | AND Expr2

data Expr3 = Expr3 Expr4 RestExpr3

data RestExpr3 = RE3eps | CompOp CompOp Expr3

data Expr4 = Expr4 Expr5 RestExpr4

data RestExpr4 = RE4eps | Plus Expr5 | Minus Expr5

data Expr5 = Expr5 Expr6 | NegExpr5 Expr6

data Expr6 = Expr6 Expr7 RestExpr6

data RestExpr6 = RE6eps | Mult Expr7 | Div Expr7

data Expr7 = Expr7 AtomicExpr RestExpr7

data RestExpr7 = RE7eps | RestAtomic Expr7

data AtomicExpr = AtomExpr AtomExpr | Parenthesised Expr

{-
data Token
  = Number String 
  | Plus
  | RoundBracketOpen
  | RoundBracketClose
  deriving (Show, Eq)

data Expression
  = Literal String
  | Addition Expression Expression
  deriving (Show)


Backus–Naur form

expression ::=
  Number restExpression |
  RoundBracketOpen restExpression RoundBracketClose restExpression

restExpression ::= Plus expression | \epsilon

-}

data CompOp
  = CompEq
  | CompSmaller

newtype Var = Name String
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
  deriving (Show, Eq)

data Context = CtxDef | CtxLocalDef | CtxOther
  deriving (Show)

type Parser token a = [token] -> (Maybe a, [token])

expression :: Parser Token Expr
expression (Number text : tokensRest0) =
  case plusExpression tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1)
    (Just plusExpressionResult, tokensRest1) ->
      (Just $ addExpression (Literal text) plusExpressionResult, tokensRest1)
expression (RoundBracketOpen : tokensRest0) =
  case expression tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1)
    (Just expressionResult, RoundBracketClose : tokensRest1) ->
      case plusExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, tokensRest2)
        (Just plusExpressionResult, tokensRest2) ->
          ( Just $ addExpression expressionResult plusExpressionResult,
            tokensRest2
          )
    (Just _, tokensRest1) -> (Nothing, tokensRest1)
expression tokens = (Nothing, tokens)

data PlusExpression = NoPlus | JustPlus Expression

addExpression :: Expression -> PlusExpression -> Expression
addExpression e NoPlus = e
addExpression e0 (JustPlus e1) = Addition e0 e1

plusExpression :: Parser Token PlusExpression
plusExpression (Plus : tokensRest0) =
  case expression tokensRest0 of
    (result, tokensRest1) -> (JustPlus <$> result, tokensRest1)
plusExpression tokens = (Just NoPlus, tokens)

test =
  expression
    [ Number "2",
      Plus,
      RoundBracketOpen,
      RoundBracketOpen,
      Number "3",
      RoundBracketClose,
      Plus,
      Number "5",
      RoundBracketClose
    ]

main :: IO ()
main = Relude.print test


-- ATTEMPT 4

-- parseExpr :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
-- parseExpr ctx (Left x) = Left x
-- parseExpr ctx (Right []) = Right []
-- parseExpr ctx (Right all@(T_LET : rest)) = parseLet ctx $ Right all
-- parseExpr ctx (Right all@(T_IN : rest)) = parseIn ctx $ Right all
-- parseExpr ctx (Right all@(T_IF : rest)) = parseIf ctx $ Right all -- TODO: continue here
-- parseExpr ctx (Right all@(T_ELSE : rest)) = parseElse ctx $ Right all
-- parseExpr ctx (Right all@(T_THEN : rest)) = parseThen ctx $ Right all
-- parseExpr CtxDef (Right (TSEMICOL : rest)) = CtxDef $ Right rest
-- parseExpr CtxLocalDef (Right (TSEMICOL : rest)) = parseLocalDefinitions CtxLocalDef $ Right rest
-- parseExpr ctx (Right (TAtomExpr _ : rest)) = parseExpr ctx $ Right rest
-- parseExpr ctx (Right (TLPAREN : rest)) = parseExpr ctx $ Right rest
-- parseExpr ctx (Right (TRPAREN : rest)) = parseExpr ctx $ Right rest
-- parseExpr ctx other = parseExpr1 ctx other
