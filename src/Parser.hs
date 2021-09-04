-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser where
import Data.Text
import Data.Maybe
import Helpers
import Lexer
import DataStructures
import Debug.Trace

type Parser token a = [token] -> (Maybe a, [token])

tokensTest1 = genListOfTokens "id a = a; main = id 0;"
tokensTest2 = genListOfTokens "k1 a b = b; main = k1 0 1;"
tokensTest3 = genListOfTokens "fun a b c = a; main = fun 1 2 3;"
tokensTest4 = genListOfTokens "fun a b c d = a; main = fun 1 2 (3 + 2) 4;"

------------------------------- PROGRAM -------------------------------

parseProgram :: Parser Token Ast
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

parseDefinition :: Parser Token SubTree
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

------------------------------- EXPRESIONS -------------------------------

parseExpression :: Parser Token Expr
parseExpression (T_LET : tokensRest0) =
  case parseLocalDefs tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseLocalDefs returned Nothing (in parseExpression, LET case)" : tokensRest1)
    (Just localDefs, tokensRest1) ->
      case parseExpression tokensRest1 of
        (Nothing, tokensRest2) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, LET case)" : tokensRest2)
        (Just expressionResult, tokensRest2) ->
          (Just $ LetX localDefs expressionResult, tokensRest2)
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
              (Just $ IfX firstExpressionResult secondExpressionResult thirdExpressionResult, tokensRest3)
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
        (Just (OR expr1), tokensRest1) -> (Just (OrX expr2 expr1), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest1 returned Nothing" : tokensRest1)

parseRest1 :: Parser Token RestExpr1
parseRest1 (TBinOp BO_OR : tokensRest0) =
  case parseExpr1 tokensRest0 of
    (expr1, tokensRest1) -> (OR <$> expr1, tokensRest1)
parseRest1 tokens = (Just RE1eps, tokens)

----------------------------------------------------------------------------------------

parseExpr2 :: Parser Token  Expr
parseExpr2 tokens =
  case parseExprNot tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr3 returned Nothing" : rest)
    (Just expr3, tokensRest0) -> 
      case parseRest2 tokensRest0 of 
        (Just RE2eps, tokensRest1) -> (Just expr3, tokensRest1)
        (Just (AND expr2), tokensRest1) -> (Just (AndX expr3 expr2), tokensRest1)
        (Nothing, tokensRest1) -> (Nothing, Error "parseRest2 returned Nothing" : tokensRest1)


parseRest2 :: Parser Token RestExpr2
parseRest2 (TBinOp BO_AND : tokensRest0) =
  case parseExpr2 tokensRest0 of
    (expr3, tokensRest1) -> (AND <$> expr3, tokensRest1)
parseRest2 tokens = (Just RE2eps, tokens)

----------------------------------------------------------------------------------------

parseExprNot :: Parser Token  Expr
parseExprNot all@(TUniOp UO_NOT : tokensRest0) =
  case parseExpr3 tokensRest0 of
    (Nothing, tokensRest1) -> (Nothing, Error "parseExpr3 returned Nothing (Not Case)" : tokensRest1)
    (Just expr3, tokensRest1) -> (Just (NotX expr3), tokensRest1)
parseExprNot tokens = 
  case parseExpr3 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr3 returned Nothing (Pos Case)" : rest)
    (Just expr3, tokensRest) -> (Just expr3, tokensRest)
    
----------------------------------------------------------------------------------------

parseExpr3 :: Parser Token Expr
parseExpr3 tokens =
  case parseExpr4 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr4 returned Nothing" : rest)
    (Just expr4, tokensRest0) ->
      case parseRest3 tokensRest0 of 
        (Just RE3eps, tokensRest1) -> (Just expr4, tokensRest1)
        (Just (CompEq expr3), tokensRest1) -> (Just (EqualsX expr4 expr3), tokensRest1)
        (Just (CompSmaller expr3), tokensRest1) -> (Just (SmallerX expr4 expr3), tokensRest1)
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
        (Just (PLUS expr4), tokensRest1) -> (Just (PlusX expr5 expr4), tokensRest1)
        (Just (MINUS expr4), tokensRest1) -> (Just (MinusX expr5 expr4), tokensRest1)
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
    (Just expr6, tokensRest1) -> (Just (NegX expr6), tokensRest1)
parseExpr5 tokens = 
  case parseExpr6 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr6 returned Nothing (Pos Case)" : rest)
    (Just expr6, tokensRest) -> (Just expr6, tokensRest)

----------------------------------------------------------------------------------------

parseExpr6 :: Parser Token  Expr
parseExpr6 tokens =
  case parseExpr7 tokens of
    (Nothing, rest) -> (Nothing, Error "parseExpr7 returned Nothing" : rest)
    (Just expr7, tokensRest0) -> 
      case parseRest6 tokensRest0 of 
        (Just RE6eps, tokensRest1) -> (Just expr7, tokensRest1)
        (Just (MULT expr6), tokensRest1) -> (Just (MultX expr7 expr6), tokensRest1)
        (Just (DIV expr6), tokensRest1) -> (Just (DivX expr7 expr6), tokensRest1)
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
  case parseExpr7' tokens of
    (Nothing, rest) -> (Nothing, Error "parseAtomicExpr7' returned Nothing" : rest)
    (Just all@(x:xs), rest) -> (Just (leftAssociate all), rest)
    (Just [], rest) -> (Just EmptyExpr, rest) 

leftAssociate :: [Expr] -> Expr
leftAssociate (x:xs:xss) = leftAssociate' (AppX x xs) xss
leftAssociate [x] = x
leftAssociate [] = error "Error in leftAssociate: [] not expected here"

leftAssociate' :: Expr -> [Expr] -> Expr 
leftAssociate' new (x:xs:xss:xsss)      = leftAssociate' (AppX new xs) xsss
leftAssociate' new [left, right]        = AppX new right
leftAssociate' new [last]               = AppX new last
leftAssociate' _ other                  = error $ "leftAssociate does not expects this kind of Expression here: " ++ show other

parseExpr7' :: Parser Token  [Expr]
parseExpr7' tokens =
  case parseAtomicExpr tokens of
    (Nothing, rest) -> (Nothing, Error "parseAtomicExpr returned Nothing" : rest)
    (Just expr, tokensRest0) -> 
      case parseRest7 tokensRest0 of
        (Just [], tokensRest1)          -> (Just [expr], tokensRest1)
        (Just expr7, tokensRest1)       -> (Just (expr : expr7), tokensRest1)
        (Nothing, tokensRest1)          -> (Nothing, Error "parseRest7 returned Nothing" : tokensRest1)

parseRest7 :: Parser Token [Expr]
parseRest7 [] = (Nothing, [Error "parseRest7 should never be called with empty list of tokens, maybe a semicolon is missing?"])
parseRest7 (TSEMICOL : tokensRest0) = (Just [], tokensRest0)
parseRest7 (T_THEN : tokensRest0) = (Just [], tokensRest0)
parseRest7 (T_ELSE : tokensRest0) = (Just [], tokensRest0)
parseRest7 all@(T_IN : tokensRest0) = (Just [], all)
parseRest7 all@(TRPAREN : tokensRest0) = (Just [], all)
-- parseRest7 all@(TRPAREN : tokensRest0) = (Just RE7eps, all)
parseRest7 all@(next : tokens)
                      | isOperator next = (Just [], all)
                      | otherwise = case parseExpr7' all of
                        (Nothing, tokensRest) -> (Nothing, Error "Error in parseRest7: " : tokensRest)
                        (expr7, tokensRest) -> (expr7, tokensRest)

----------------------------------------------------------------------------------------

isOperator :: Token -> Bool 
isOperator (TBinOp op)  = True 
isOperator (TUniOp op)  = True
isOperator _            = False  

----------------------------------------------------------------------------------------

parseAtomicExpr :: Parser Token Expr
parseAtomicExpr (TAtomExpr (T_VAR (Name name)) : tokensRest) = (Just (VarX name), tokensRest)
parseAtomicExpr (TAtomExpr (T_INT int) : tokensRest) = (Just (IntX int), tokensRest)
parseAtomicExpr (TAtomExpr (T_BOOL bool) : tokensRest) = (Just (BoolX bool), tokensRest)
parseAtomicExpr (TLPAREN : tokensRest0) = case parseExpression tokensRest0 of 
  (Nothing, tokensRest1) -> (Nothing, Error "parseExpression returned Nothing (in parseExpression, ELSE case)" : tokensRest1)
  (expr, TRPAREN : tokensRest1) -> (expr, tokensRest1)
  (expr, tokensRest1) -> (expr, tokensRest1)
parseAtomicExpr tokens = (Nothing, Error "parseAtomicExpr was called with the following unsupported token: " : tokens)

--------------------------------- HELPER ---------------------------------

parseEqualSign :: Parser Token Token
parseEqualSign (TEQUAL : tokensRest0) = (Just TEQUAL, tokensRest0)
parseEqualSign tokens = (Nothing, Error "parseEqualSign was called with the following unaccepted token: " : tokens)

----------------------------------------------------------------------------------------
-- Some tests for left association:

wrong3 :: Expr
wrong3   = AppX (VarX "fun") (AppX (IntX 1) (AppX (IntX 2) (IntX 3)))
correct3 :: Expr
correct3 = AppX (AppX (AppX (VarX "fun") (IntX 1)) (IntX 2)) (IntX 3)

testProg1Corr :: [SubTree]
testProg1Corr  = [FuncDef "id" ["a"] (VarX "a"),VarDef "main" (AppX (VarX "id") (IntX 0))]

testProg2False :: [SubTree]
testProg2False = [FuncDef "k1" ["a","b"] (VarX "a"), VarDef "main" (AppX (VarX "k1") (AppX (IntX 0) (IntX 1)))]
testProg2Corr :: [SubTree]
testProg2Corr  = [FuncDef "k1" ["a", "b"] (VarX "a"), FuncDef "main" [] (AppX (AppX (VarX "k1") (IntX 0)) (IntX 1))]
              -- [FuncDef "k1" ["a","b"] (VarX "b"),  VarDef  "main"    (AppX (AppX (VarX "k1") (IntX 0)) (IntX 1))]

testProg3False :: [SubTree]
testProg3False      = [FuncDef "fun" ["a","b","c"] (VarX "a"),VarDef "main" (AppX (VarX "fun") (AppX (IntX 1) (AppX (IntX 2) (IntX 3))))]
testProg3AlsoFalse :: [SubTree]
testProg3AlsoFalse  = [FuncDef "fun" ["a","b","c"] (VarX "a"),VarDef "main" (AppX (AppX (VarX "fun") (AppX (IntX 1) (IntX 2))) (IntX 3))]
                  --  [FuncDef "fun" ["a","b","c"] (VarX "a"),VarDef "main" (AppX (AppX (VarX "fun") (AppX (IntX 1) (IntX 2))) (IntX 2))]
lel :: [SubTree]
lel =  [FuncDef "fun" ["a","b","c"] (VarX "a"),VarDef "main" (AppX (AppX (AppX (IntX 2) (IntX 1)) (VarX "fun")) (IntX 3))]
testProg3Corr :: [SubTree]
testProg3Corr       = [FuncDef "fun" ["a","b","c"] (VarX "a"),VarDef "main" (AppX (AppX (AppX (VarX "fun") (IntX 1)) (IntX 2)) (IntX 3))]
                                                                         -- (AppX (AppX (AppX (VarX "fun") (IntX 1)) (IntX 2)) (IntX 3))