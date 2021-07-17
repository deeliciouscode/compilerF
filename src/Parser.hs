-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser where

import Relude
import Data.Text

import Helpers
import Lexer
import DataStructures

type Parser token a = [token] -> (Maybe a, [token])

parseExpression :: Parser Token Expr
parseExpression (T_LET : tokensRest0) =
  case parseLocalDefs tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1)
    (Just localDefs, tokensRest1) ->
      case parseExpression tokensRest1 of 
        (Nothing, tokensRest2) -> (Nothing, tokensRest2)
        (Just expressionResult, tokensRest2) -> 
          (Just $ LetIn localDefs expressionResult, tokensRest1)
parseExpression (T_IF : tokensRest0) =
  case parseExpression tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1) -- check if we need to check for then and else
    (Just firstExpressionResult, tokensRest1) -> 
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, tokensRest2)
        (Just secondExpressionResult, tokensRest2) ->
          case parseExpression tokensRest2 of
            (Nothing, tokensRest3) -> (Nothing, tokensRest3)
            (Just thirdExpressionResult, tokensRest3) ->
              (Just $ IfElseThen firstExpressionResult secondExpressionResult thirdExpressionResult, tokensRest3)
parseExpression all@(TUniOp UO_MINUS : tokensRest0) = 
  case parseExpr5 all of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1)
    (Just (NegExpr5 expr), tokensRest1) ->
      (Just $ UnMinus expr, tokensRest1)
    (Just other, tokensRest1) -> (Nothing, tokensRest1)
parseExpression (TAtomExpr atom : tokensRest0) =
  case parseRest1 tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1)
    (Just (OR restOr), tokensRest1) -> 
      (Just $ Or (atomToExpr atom) restOr, tokensRest1)
    (Just RE1eps, tokensRest1) ->
      case parseRest2 tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, tokensRest2)
        (Just (AND restAnd), tokensRest2) -> 
          (Just $ And (atomToExpr atom) restAnd, tokensRest2)
        (Just RE2eps, tokensRest2) ->
          case parseRest3 tokensRest2 of
            (Nothing, tokensRest3) -> (Nothing, tokensRest3)
            (Just (CompEq' restComp), tokensRest3) -> 
              (Just $ CompEq (atomToExpr atom) restComp, tokensRest3)
            (Just (CompSmaller' restComp), tokensRest3) -> 
              (Just $ CompSmaller (atomToExpr atom) restComp, tokensRest3)
            (Just RE3eps, tokensRest3) -> 
              case parseRest4 tokensRest3 of
                (Nothing, tokensRest4) -> (Nothing, tokensRest4)
                (Just (PLUS restPlus), tokensRest4) -> 
                  (Just $ Plus (atomToExpr atom) restPlus, tokensRest4)
                (Just (MINUS restMinus), tokensRest4) -> 
                  (Just $ Minus (atomToExpr atom) restMinus, tokensRest4)
                (Just RE4eps, tokensRest4) ->
                  case parseExpr5 tokensRest4 of
                    (Nothing, tokensRest5) -> (Nothing, tokensRest5)
                    (Just (NegExpr5 expr), tokensRest5) -> 
                      (Just $ UnMinus expr , tokensRest5)
                    (Just NotNeg, tokensRest5) -> 
                      case parseRest6 tokensRest5 of
                        (Nothing, tokensRest6) -> (Nothing, tokensRest6)
                        (Just (MULT restMult), tokensRest6) -> 
                          (Just $ Times (atomToExpr atom) restMult, tokensRest6)
                        (Just (DIV restDiv), tokensRest6) -> 
                          (Just $ Divided (atomToExpr atom) restDiv, tokensRest6)
                        (Just RE6eps, tokensRest6) -> 
                          case parseRest7 tokensRest6 of
                            (Nothing, tokensRest7) -> (Nothing, tokensRest7)
                            (Just (FULL expr), tokensRest7) -> 
                              (Just $ Appl (atomToExpr atom) expr, tokensRest7)
                            (Just RE7eps, tokensRest7) -> (Just $ atomToExpr atom, tokensRest7)

parseExpression tokens = (Nothing, tokens)


---------------------------- EXPRESSION 1 - 7 ----------------------------

parseRest1 :: Parser Token RestExpr1
parseRest1 (TBinOp BO_OR : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (OR <$> expr, tokensRest1)
parseRest1 tokens = (Just RE1eps, tokens)

parseRest2 :: Parser Token RestExpr2
parseRest2 (TBinOp BO_AND : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (AND <$> expr, tokensRest1)
parseRest2 tokens = (Just RE2eps, tokens)

parseRest3 :: Parser Token RestExpr3
parseRest3 (TBinOp BO_EQUAL : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (CompEq' <$> expr, tokensRest1)
parseRest3 (TBinOp BO_SMALLER : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (CompSmaller' <$> expr, tokensRest1)
parseRest3 tokens = (Just RE3eps, tokens)

parseRest4 :: Parser Token RestExpr4
parseRest4 (TBinOp BO_PLUS : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (PLUS <$> expr, tokensRest1)
parseRest4 (TBinOp BO_MINUS : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (MINUS <$> expr, tokensRest1)
parseRest4 tokens = (Just RE4eps, tokens)

parseExpr5 :: Parser Token Expr5
parseExpr5 (TUniOp UO_MINUS : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (NegExpr5 <$> expr, tokensRest1)
parseExpr5 tokens = (Just NotNeg, tokens)

parseRest6 :: Parser Token RestExpr6
parseRest6 (TBinOp BO_MUL : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (MULT <$> expr, tokensRest1)
parseRest6 (TBinOp BO_DIV : tokensRest0) = 
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (DIV <$> expr, tokensRest1)
parseRest6 tokens = (Just RE6eps, tokens)
 
parseRest7 :: Parser Token RestExpr7
parseRest7 (TSEMICOL : tokensRest0) = (Just RE7eps, tokensRest0)
parseRest7 tokens =  
  case parseExpression tokens of
    (expr, tokensRest0) -> (FULL <$> expr, tokensRest0) 

atomToExpr :: AtomExpr -> Expr
atomToExpr (T_VAR (Name name)) = Variable name
atomToExpr (T_INT number) = Int number
atomToExpr (T_BOOL bool) = Bool bool 


------------------------------- LOCAL DEFS -------------------------------

parseLocalDefs :: Parser Token LocDefs
parseLocalDefs all@(TAtomExpr (T_VAR (Name name)) : tokensRest0) = 
  case parseLocalDef all of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1)
    (Just locDef, tokensRest1) -> 
      case parseRestLocalDefs tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, tokensRest2)
        (Just restLocDefs, tokensRest2) -> 
          (Just (LocDefs locDef restLocDefs), tokensRest1)
parseLocalDefs tokens = (Nothing, tokens)

parseLocalDef :: Parser Token LocDef
parseLocalDef (TAtomExpr (T_VAR (Name name)) : tokensRest0) = 
  case parseEqualSign tokensRest0 of 
    (Nothing , tokensRest1) -> (Nothing , tokensRest1)
    (Just TEQUAL, tokensRest1) -> 
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, tokensRest2)
        (Just expr, tokensRest2) -> (Just (LocDef (Name name) expr), tokensRest2)
    (Just _, tokensRest1) -> (Nothing, tokensRest1)
parseLocalDef tokens = (Nothing, tokens)

parseRestLocalDefs :: Parser Token RestLocDefs
parseRestLocalDefs (T_IN : tokensRest0) = (Just LDeps, tokensRest0) --maybe all
parseRestLocalDefs all@(TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseLocalDefs all of
    (Nothing, tokensRest1) -> (Nothing, tokensRest1) 
    (locDefs, tokensRest1) -> (RLocDefs <$> locDefs, tokensRest1)
parseRestLocalDefs tokens = (Nothing, tokens)

parseEqualSign :: Parser Token Token
parseEqualSign (TEQUAL : tokensRest0) = (Just TEQUAL, tokensRest0)
parseEqualSign tokens = (Nothing, tokens)


---------------------------------- TEST ----------------------------------

-- Move to Interpreter.hs

main :: IO ()
main = do 
    Relude.putStrLn "Type an expression to parse: "
    string <- Relude.getLine
    let lexed = genListOfTokens $ unpack string in
        let parsed = parseExpression lexed in 
            Relude.print parsed