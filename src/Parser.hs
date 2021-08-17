-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser where

-- import Relude
import Data.Text
import Data.Maybe

import Helpers
import Lexer
import DataStructures

type Parser token a = [token] -> (Maybe a, [token])

------------------------------- PROGRAM -------------------------------

parseProgram :: Parser Token Prog
parseProgram tokens =
  case parseDefinition tokens of
    (Nothing, tokensRest0) -> (Nothing, Error "parseProgram returned Nothing" : tokensRest0)
    (Just def, tokens) -> (Just (Prog def (fromJust (fst (parseRestProgram tokens)))), [])

parseRestProgram :: Parser Token RestProg
parseRestProgram [] = (Just Deps, [])
parseRestProgram tokens = (Just (RProg (fromJust (fst restProg))), snd restProg)
                where restProg = parseProgram tokens

------------------------------- DEFINITION -------------------------------

parseDefinition :: Parser Token Def
parseDefinition (TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseArgs tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseDefinition returned Nothing (in parseExpression, LET case)" : tokensRest1)
    (Just args, tokensRest1) ->
      case parseEqualSign tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseDefinition returned Nothing (in parseExpression, LET case)" : tokensRest2)
        (Just TEQUAL, tokensRest2) ->
          case parseExpression tokensRest2 of
            (Nothing, tokensRest3) -> (Nothing, Error "parseExpression returned Nothing (in parseDefinition, Atom case)" : tokensRest3)
            (Just expr, tokensRest3) -> (Just (Def (Name name) args expr), tokensRest3)
        (Just _, tokensRest2) -> (Nothing, Error "parseEqualSign returned something other than TEQUAL (in parseDefinition, Atom case)" : tokensRest2)
parseDefinition tokens = (Nothing, Error "parseDefinition was called with the following unsupported token: " : tokens)

parseArgs :: Parser Token Args
parseArgs (TAtomExpr (T_VAR (Name name)) : tokensRest0) = (Just (RVars (Name name) (fromJust (fst args))), snd args)
              where args = parseArgs tokensRest0
parseArgs all@(TEQUAL : tokensRest0) = (Just Veps, all)
parseArgs tokens = (Nothing, Error "parseArgs was called with the following unsupported token: " : tokens)

------------------------------- EXPRESIONS -------------------------------

parseExpression :: Parser Token Expr
parseExpression (T_LET : tokensRest0) =
  case parseLocalDefs tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseLocalDefs returned Nothing (in parseExpression, LET case)" : tokensRest1)
    (Just localDefs, tokensRest1) ->
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, LET case)" : tokensRest2)
        (Just expressionResult, tokensRest2) ->
          (Just $ LetIn localDefs expressionResult, tokensRest2)
parseExpression (T_IF : tokensRest0) =
  case parseExpression tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, IF case)" : tokensRest1) -- check if we need to check for then and else
    (Just firstExpressionResult, tokensRest1) ->
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, THEN case)" : tokensRest2)
        (Just secondExpressionResult, tokensRest2) ->
          case parseExpression tokensRest2 of
            (Nothing, tokensRest3) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, ELSE case)" : tokensRest3)
            (Just thirdExpressionResult, tokensRest3) ->
              (Just $ IfThenElse firstExpressionResult secondExpressionResult thirdExpressionResult, tokensRest3)
-- I guess we have to stuff in parenthesised expresions somewhere here
-- actually it has to be implemented in parseExpr7, most likely
parseExpression tokens =
  case parseExpr1 tokens of
    (Nothing, tokensRest0) -> (Nothing, Error "parseExpr1 returned Nothing (in parseExpression, Atom case)" : tokensRest0)
    (expr1, tokensRest0) -> (Expr1' <$> expr1, tokensRest0)

---------------------------- EXPRESSION 1 - 7 ----------------------------

parseExpr1 :: Parser Token  Expr1
parseExpr1 tokens =
  case parseExpr2 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr2 returned Nothing" : rest)
    (Just expr2, tokensRest) -> (Just (Expr1 expr2 (fromJust (fst restExpr1))), snd restExpr1)
                  where restExpr1 = parseRest1 tokensRest

parseRest1 :: Parser Token RestExpr1
parseRest1 (TBinOp BO_OR : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (OR <$> expr, tokensRest1)
parseRest1 tokens = (Just RE1eps, tokens)

----------------------------------------------------------------------------------------

parseExpr2 :: Parser Token  Expr2
parseExpr2 tokens =
  case parseExpr3 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr3 returned Nothing" : rest)
    (Just expr3, tokensRest) -> (Just (Expr2 expr3 (fromJust (fst restExpr2))), snd restExpr2)
                  where restExpr2 = parseRest2 tokensRest

parseRest2 :: Parser Token RestExpr2
parseRest2 (TBinOp BO_AND : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (AND <$> expr, tokensRest1)
parseRest2 tokens = (Just RE2eps, tokens)

----------------------------------------------------------------------------------------

parseExpr3 :: Parser Token  Expr3
parseExpr3 tokens =
  case parseExpr4 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr4 returned Nothing" : rest)
    (Just expr4, tokensRest) -> (Just (Expr3 expr4 (fromJust (fst restExpr3))), snd restExpr3)
                  where restExpr3 = parseRest3 tokensRest

parseRest3 :: Parser Token RestExpr3
parseRest3 (TBinOp BO_EQUAL : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (CompEq' <$> expr, tokensRest1)
parseRest3 (TBinOp BO_SMALLER : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (CompSmaller' <$> expr, tokensRest1)
parseRest3 tokens = (Just RE3eps, tokens)

----------------------------------------------------------------------------------------

parseExpr4 :: Parser Token  Expr4
parseExpr4 tokens =
  case parseExpr5 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr5 returned Nothing" : rest)
    (Just expr5, tokensRest) -> (Just (Expr4 expr5 (fromJust (fst restExpr4))), snd restExpr4)
                  where restExpr4 = parseRest4 tokensRest

parseRest4 :: Parser Token RestExpr4
parseRest4 (TBinOp BO_PLUS : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (PLUS <$> expr, tokensRest1)
parseRest4 (TBinOp BO_MINUS : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (MINUS <$> expr, tokensRest1)
parseRest4 tokens = (Just RE4eps, tokens)

----------------------------------------------------------------------------------------

parseExpr5 :: Parser Token  Expr5
parseExpr5 all@(TUniOp UO_MINUS : tokensRest0) =
  case parseExpr6 tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseExpr6 returned Nothing (Neg Case)" : tokensRest1)
    (Just expr6, tokensRest1) -> (Just (NegExpr5 expr6), tokensRest1)
parseExpr5 tokens = 
  case parseExpr6 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr6 returned Nothing (Pos Case)" : rest)
    (Just expr6, tokensRest) -> (Just (PosExpr5 expr6), tokensRest)

----------------------------------------------------------------------------------------

parseExpr6 :: Parser Token  Expr6
parseExpr6 tokens =
  case parseExpr7 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr7 returned Nothing" : rest)
    (Just expr7, tokensRest) -> (Just (Expr6 expr7 (fromJust (fst restExpr6))), snd restExpr6)
                  where restExpr6 = parseRest6 tokensRest

parseRest6 :: Parser Token RestExpr6
parseRest6 (TBinOp BO_MUL : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (MULT <$> expr, tokensRest1)
parseRest6 (TBinOp BO_DIV : tokensRest0) =
  case parseExpression tokensRest0 of
    (expr, tokensRest1) -> (DIV <$> expr, tokensRest1)
parseRest6 tokens = (Just RE6eps, tokens)

----------------------------------------------------------------------------------------

parseExpr7 :: Parser Token  Expr7
parseExpr7 tokens =
  case parseAtomicExpr tokens of
    (Nothing, rest) -> (Nothing, Error "parseAtomicExpr returned Nothing" : rest)
    (Just atomExpr, tokensRest) -> (Just (Expr7 atomExpr (fromJust (fst restExpr7))), snd restExpr7)
                  where restExpr7 = parseRest7 tokensRest

parseRest7 :: Parser Token RestExpr7
parseRest7 [] = (Nothing, [Error "parseRest7 should never be called with empty list of tokens, maybe a semicolon is missing?"])
parseRest7 (TSEMICOL : tokensRest0) = (Just RE7eps, tokensRest0)
parseRest7 all@(next : tokens)
                      | isOperator next = (Just RE7eps, all)
                      | otherwise = case parseExpr7 all of
                        (Nothing, tokensRest) -> (Nothing, Error "Error in parseRest7: " : tokensRest)
                        (expr7, tokensRest) -> (App <$> expr7, tokensRest)

----------------------------------------------------------------------------------------

isOperator :: Token -> Bool 
isOperator (TBinOp op)  = True 
isOperator (TUniOp op)  = True
isOperator _            = False  

----------------------------------------------------------------------------------------

parseAtomicExpr :: Parser Token  AtomicExpr
parseAtomicExpr (TAtomExpr atomExpr : tokensRest) =
  (Just (AtomExpr atomExpr), tokensRest)
parseAtomicExpr tokens = (Nothing, Error "parseAtomicExpr was called with the following unsupported token: " : tokens)
-- parseAtomicExpr (TLPAREN : tokensRest) = ()
-- parseAtomicExpr tokens = 
--   case parseAtomicExpr tokens of
--     (Nothing, rest) -> (Nothing, Error "parseExpr1 returned Nothing" : rest)
--     (Just atomExpr, tokensRest) -> (Just (Expr7 atomExpr (fromJust (fst restExpr7))), snd restExpr7)
--                   where restExpr7 = parseRest7 tokensRest


------------------------------- LOCAL DEFS -------------------------------

parseLocalDefs :: Parser Token LocDefs
parseLocalDefs all@(TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseLocalDef all of
    (Nothing, tokensRest1) -> (Nothing, Error "parseLocalDef returned Nothing (in parseLocalDefs, Atom case)" : tokensRest1)
    (Just locDef, tokensRest1) ->
      case parseRestLocalDefs tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseRestLocalDefs returned Nothing (in parseLocalDefs, Atom case)" : tokensRest2)
        (Just restLocDefs, tokensRest2) ->
          (Just (LocDefs locDef restLocDefs), tokensRest2)
parseLocalDefs tokens = (Nothing, Error "parseLocalDefs was called with a the following wrong token: " : tokens)

parseLocalDef :: Parser Token LocDef
parseLocalDef (TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseEqualSign tokensRest0 of
    (Nothing , tokensRest1) -> (Nothing, Error "parseEqualSign returned Nothing (in parseLocalDef, Atom case)" : tokensRest1)
    (Just TEQUAL, tokensRest1) ->
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseExpression returned Nothing (in parseLocalDef, Atom case)" : tokensRest2)
        (Just expr, tokensRest2) -> (Just (LocDef (Name name) expr), tokensRest2)
    (Just _, tokensRest1) -> (Nothing, Error "parseEqualSign returned something other than TEQUAL (in parseLocalDef, Atom case)" : tokensRest1)
parseLocalDef tokens = (Nothing, Error "parseLocalDef was called with the following unsupported token: " : tokens)

parseRestLocalDefs :: Parser Token RestLocDefs
parseRestLocalDefs (T_IN : tokensRest0) = (Just LDeps, tokensRest0) --maybe all
parseRestLocalDefs all@(TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseLocalDefs all of
    (Nothing, tokensRest1) -> (Nothing, Error "parseLocalDefs returned Nothing (in parseRestLocalDefs, Atom case)" : tokensRest1)
    (locDefs, tokensRest1) -> (RLocDefs <$> locDefs, tokensRest1)
parseRestLocalDefs tokens = (Nothing, Error "parseRestLocalDefs was called with the following unaccepted token: " : tokens)

--------------------------------- HELPER ---------------------------------

parseEqualSign :: Parser Token Token
parseEqualSign (TEQUAL : tokensRest0) = (Just TEQUAL, tokensRest0)
parseEqualSign tokens = (Nothing, Error "parseEqualSign was called with the following unaccepted token: " : tokens)

checkForEndToken :: Parser Token EndToken
checkForEndToken all@(T_IN : tokensRest0) = (Just EndToken, all)
checkForEndToken all@(TSEMICOL : tokensRest0) = (Just EndToken, tokensRest0)
checkForEndToken all@(T_THEN : tokensRest0) = (Just EndToken, tokensRest0)
checkForEndToken all@(T_ELSE : tokensRest0) = (Just EndToken, tokensRest0)
checkForEndToken tokens = (Nothing, tokens)
