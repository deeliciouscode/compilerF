-- module Parser3 where

-- import DataStructures
-- import Helpers

-- parseProgram :: [Token] -> Either InvalidSyntaxError [Token]

module Parser3 where

import Relude

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

{-
Backus–Naur form

expression ::=
  Number restExpression |
  RoundBracketOpen expression RoundBracketClose restExpression

restExpression ::= Plus expression | \epsilon

-}

type Parser token a = [token] -> (Maybe a, [token])

expression :: Parser Token Expression
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
    [ 
      Number "2",
      Plus,
      Number "3"
    ]

-- [ 
--       Number "2",
--       Plus,
--       RoundBracketOpen,
--       RoundBracketOpen,
--       Number "3",
--       RoundBracketClose,
--       Plus,
--       Number "5",
--       RoundBracketClose
--     ]

main :: IO ()
main = Relude.print test

-- $> ArithmeticParserVerbose.main
