-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser where
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
    (Just Deps, tokensRest0) -> (Just [], tokensRest0) 
    (Just def, tokensRest0) -> (Just (def : restDefs), tokensRest1)
                where
                  parsedRestProgram = parseProgram tokensRest0
                  restDefs = fromMaybe [] $ fst parsedRestProgram
                  tokensRest1 = snd parsedRestProgram

------------------------------- DEFINITION -------------------------------

parseDefinition :: Parser Token Def
parseDefinition (TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseArgs tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseArgs returned Nothing" : tokensRest1)
    (Just args, tokensRest1) ->
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseExpression returned Nothing (in parseDefinition, Atom case)" : tokensRest2)
        (Just expr, tokensRest2)
                      | Prelude.null args -> (Just (VarDef name expr), tokensRest2)
                      | otherwise -> (Just (FuncDef name args expr), tokensRest2)
parseDefinition [] = (Just Deps, [])
parseDefinition tokens = (Nothing, Error "parseDefinition was called with the following unsupported token: " : tokens)

parseArgs :: Parser Token Args
parseArgs (TAtomExpr (T_VAR (Name name)) : tokensRest0) = (Just (name : args), tokensRest1)
              where
                parsedArgs = parseArgs tokensRest0
                args = fromMaybe [] $ fst parsedArgs
                tokensRest1 = snd parsedArgs            
parseArgs all@(TEQUAL : tokensRest0) = (Just [], tokensRest0)
parseArgs tokens = (Nothing, Error "parseArgs was called with the following unsupported token: " : tokens)


------------------------------- LOCAL DEFS -------------------------------

parseLocalDefs :: Parser Token LocDefs
parseLocalDefs (T_IN : tokensRest0) = (Just [], tokensRest0)
parseLocalDefs all@(TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseLocalDef all of
    (Nothing, tokensRest1)          -> (Nothing, Error "parseLocalDef returned Nothing (in parseLocalDefs, Atom case)" : tokensRest1)
    (Just LDeps, tokensRest1)       -> (Just [], tokensRest1)
    (Just locDef, tokensRest1)      -> (Just (locDef : restLocDefs), tokensRest2)
                      where
                        parsedRestLocDefs = parseLocalDefs tokensRest1
                        restLocDefs = fromMaybe [] $ fst parsedRestLocDefs
                        tokensRest2 = snd parsedRestLocDefs
parseLocalDefs tokens = (Nothing, Error "parseLocalDefs was called with a the following unsupported token: " : tokens)
                                    

parseLocalDef :: Parser Token LocDef
parseLocalDef (TAtomExpr (T_VAR (Name name)) : tokensRest0) =
  case parseEqualSign tokensRest0 of
    (Nothing , tokensRest1) -> (Nothing, Error "parseEqualSign returned Nothing (in parseLocalDef, Atom case)" : tokensRest1)
    (Just TEQUAL, tokensRest1) ->
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseExpression returned Nothing (in parseLocalDef, Atom case)" : tokensRest2)
        (Just expr, tokensRest2) -> (Just (LocDef name expr), tokensRest2)
    (Just _, tokensRest1) -> (Nothing, Error "parseEqualSign returned something other than TEQUAL (in parseLocalDef, Atom case)" : tokensRest1)
parseLocalDef tokens = (Nothing, Error "parseLocalDef was called with the following unsupported token: " : tokens)

-- parseRestLocalDefs :: Parser Token RestLocDefs
-- parseRestLocalDefs (T_IN : tokensRest0) = (Just LDeps, tokensRest0) --maybe all
-- parseRestLocalDefs all@(TAtomExpr (T_VAR (Name name)) : tokensRest0) =
--   case parseLocalDefs all of
--     (Nothing, tokensRest1) -> (Nothing, Error "parseLocalDefs returned Nothing (in parseRestLocalDefs, Atom case)" : tokensRest1)
--     (locDefs, tokensRest1) -> (RLocDefs <$> locDefs, tokensRest1)
-- parseRestLocalDefs tokens = (Nothing, Error "parseRestLocalDefs was called with the following unaccepted token: " : tokens)

------------------------------- EXPRESIONS -------------------------------

parseExpression :: Parser Token Expr
parseExpression (T_LET : tokensRest0) =
  case parseLocalDefs tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseLocalDefs returned Nothing (in parseExpression, LET case)" : tokensRest1)
    (Just localDefs, tokensRest1) ->
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, LET case)" : tokensRest2)
        (Just expressionResult, tokensRest2) ->
          (Just $ Let localDefs expressionResult, tokensRest2)
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
              (Just $ If firstExpressionResult secondExpressionResult thirdExpressionResult, tokensRest3)
parseExpression tokens =
  case parseExpr1 tokens of
    (Nothing, tokensRest0) -> (Nothing, Error "parseExpr1 returned Nothing (in parseExpression, Atom case)" : tokensRest0)
    (expr1, tokensRest0) -> (expr1, tokensRest0)

---------------------------- EXPRESSION 1 - 7 ----------------------------

parseExpr1 :: Parser Token Expr
parseExpr1 tokens =
  case parseExpr2 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr2 returned Nothing" : rest)
    (Just expr2, tokensRest0) ->
      case parseRest1 tokensRest0 of
        (Just RE1eps, tokensRest1) -> (Just expr2, tokensRest1)
        (Just (OR expr1), tokensRest1) -> (Just (Or expr2 expr1), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest1 returned Nothing" : tokensRest1)

parseRest1 :: Parser Token RestExpr1
parseRest1 (TBinOp BO_OR : tokensRest0) =
  case parseExpr1 tokensRest0 of
    (expr1, tokensRest1) -> (OR <$> expr1, tokensRest1)
parseRest1 tokens = (Just RE1eps, tokens)

----------------------------------------------------------------------------------------

parseExpr2 :: Parser Token  Expr
parseExpr2 tokens =
  case parseExpr3 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr3 returned Nothing" : rest)
    (Just expr3, tokensRest0) -> 
      case parseRest2 tokensRest0 of 
        (Just RE2eps, tokensRest1) -> (Just expr3, tokensRest1)
        (Just (AND expr2), tokensRest1) -> (Just (And expr3 expr2), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest2 returned Nothing" : tokensRest1)


parseRest2 :: Parser Token RestExpr2
parseRest2 (TBinOp BO_AND : tokensRest0) =
  case parseExpr2 tokensRest0 of
    (expr3, tokensRest1) -> (AND <$> expr3, tokensRest1)
parseRest2 tokens = (Just RE2eps, tokens)

----------------------------------------------------------------------------------------

parseExpr3 :: Parser Token Expr
parseExpr3 tokens =
  case parseExpr4 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr4 returned Nothing" : rest)
    (Just expr4, tokensRest0) ->
      case parseRest3 tokensRest0 of 
        (Just RE3eps, tokensRest1) -> (Just expr4, tokensRest1)
        (Just (CompEq expr3), tokensRest1) -> (Just (Equals expr4 expr3), tokensRest1)
        (Just (CompSmaller expr3), tokensRest1) -> (Just (Smaller expr4 expr3), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest3 returned Nothing" : tokensRest1)

parseRest3 :: Parser Token RestExpr3
parseRest3 (TBinOp BO_EQUAL : tokensRest0) =
  case parseExpr3 tokensRest0 of
    (expr4, tokensRest1) -> (CompEq <$> expr4, tokensRest1)
parseRest3 (TBinOp BO_SMALLER : tokensRest0) =
  case parseExpr3 tokensRest0 of
    (expr4, tokensRest1) -> (CompSmaller <$> expr4, tokensRest1)
parseRest3 tokens = (Just RE3eps, tokens)

----------------------------------------------------------------------------------------

parseExpr4 :: Parser Token  Expr
parseExpr4 tokens =
  case parseExpr5 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr5 returned Nothing" : rest)
    (Just expr5, tokensRest0) -> 
      case parseRest4 tokensRest0 of 
        (Just RE4eps, tokensRest1) -> (Just expr5, tokensRest1)
        (Just (PLUS expr4), tokensRest1) -> (Just (Plus expr5 expr4), tokensRest1)
        (Just (MINUS expr4), tokensRest1) -> (Just (Minus expr5 expr4), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest4 returned Nothing" : tokensRest1)

parseRest4 :: Parser Token RestExpr4
parseRest4 (TBinOp BO_PLUS : tokensRest0) =
  case parseExpr4 tokensRest0 of
    (expr4, tokensRest1) -> (PLUS <$> expr4, tokensRest1)
parseRest4 (TBinOp BO_MINUS : tokensRest0) =
  case parseExpr5 tokensRest0 of
    (expr5, tokensRest1) -> (MINUS <$> expr5, tokensRest1)
parseRest4 tokens = (Just RE4eps, tokens)

----------------------------------------------------------------------------------------

parseExpr5 :: Parser Token  Expr
parseExpr5 all@(TUniOp UO_MINUS : tokensRest0) =
  case parseExpr6 tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseExpr6 returned Nothing (Neg Case)" : tokensRest1)
    (Just expr6, tokensRest1) -> (Just (Neg expr6), tokensRest1)
parseExpr5 tokens = 
  case parseExpr6 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr6 returned Nothing (Pos Case)" : rest)
    (Just expr6, tokensRest) -> (Just (Pos expr6), tokensRest)

----------------------------------------------------------------------------------------

parseExpr6 :: Parser Token  Expr
parseExpr6 tokens =
  case parseExpr7 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr7 returned Nothing" : rest)
    (Just expr7, tokensRest0) -> 
      case parseRest6 tokensRest0 of 
        (Just RE6eps, tokensRest1) -> (Just expr7, tokensRest1)
        (Just (MULT expr6), tokensRest1) -> (Just (Mult expr7 expr6), tokensRest1)
        (Just (DIV expr6), tokensRest1) -> (Just (Div expr7 expr6), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest6 returned Nothing" : tokensRest1)

parseRest6 :: Parser Token RestExpr6
parseRest6 (TBinOp BO_MUL : tokensRest0) =
  case parseExpr6 tokensRest0 of
    (expr7, tokensRest1) -> (MULT <$> expr7, tokensRest1)
parseRest6 (TBinOp BO_DIV : tokensRest0) =
  case parseExpr7 tokensRest0 of
    (expr7, tokensRest1) -> (DIV <$> expr7, tokensRest1)
parseRest6 tokens = (Just RE6eps, tokens)

----------------------------------------------------------------------------------------

parseExpr7 :: Parser Token  Expr
parseExpr7 tokens =
  case parseAtomicExpr tokens of
    (Nothing, rest) -> (Nothing, Error "parseAtomicExpr returned Nothing" : rest)
    (Just atomExpr, tokensRest0) -> 
      case parseRest7 tokensRest0 of 
        (Just RE7eps, tokensRest1) -> (Just atomExpr, tokensRest1)
        (Just (APP expr7), tokensRest1) -> (Just (App atomExpr expr7), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest7 returned Nothing" : tokensRest1)

parseRest7 :: Parser Token RestExpr7
parseRest7 [] = (Nothing, [Error "parseRest7 should never be called with empty list of tokens, maybe a semicolon is missing?"])
parseRest7 (TSEMICOL : tokensRest0) = (Just RE7eps, tokensRest0)
parseRest7 (T_THEN : tokensRest0) = (Just RE7eps, tokensRest0)
parseRest7 (T_ELSE : tokensRest0) = (Just RE7eps, tokensRest0)
parseRest7 all@(T_IN : tokensRest0) = (Just RE7eps, all)
parseRest7 all@(TRPAREN : tokensRest0) = (Just RE7eps, all)
-- parseRest7 all@(TRPAREN : tokensRest0) = (Just RE7eps, all)
parseRest7 all@(next : tokens)
                      | isOperator next = (Just RE7eps, all)
                      | otherwise = case parseExpr7 all of
                        (Nothing, tokensRest) -> (Nothing, Error "Error in parseRest7: " : tokensRest)
                        (expr7, tokensRest) -> (APP <$> expr7, tokensRest)

----------------------------------------------------------------------------------------

isOperator :: Token -> Bool 
isOperator (TBinOp op)  = True 
isOperator (TUniOp op)  = True
isOperator _            = False  

----------------------------------------------------------------------------------------

parseAtomicExpr :: Parser Token Expr
parseAtomicExpr (TAtomExpr (T_VAR (Name name)) : tokensRest) = (Just (Var name), tokensRest)
parseAtomicExpr (TAtomExpr (T_INT int) : tokensRest) = (Just (Int int), tokensRest)
parseAtomicExpr (TAtomExpr (T_BOOL bool) : tokensRest) = (Just (Bool bool), tokensRest)
parseAtomicExpr (TLPAREN : tokensRest0) = case parseExpression tokensRest0 of 
  (Nothing, tokensRest1) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, ELSE case)" : tokensRest1)
  (expr, TRPAREN : tokensRest1) -> (expr, tokensRest1)
  (expr, tokensRest1) -> (expr, tokensRest1)
parseAtomicExpr tokens = (Nothing, Error "parseAtomicExpr was called with the following unsupported token: " : tokens)

--------------------------------- HELPER ---------------------------------

parseEqualSign :: Parser Token Token
parseEqualSign (TEQUAL : tokensRest0) = (Just TEQUAL, tokensRest0)
parseEqualSign tokens = (Nothing, Error "parseEqualSign was called with the following unaccepted token: " : tokens)

