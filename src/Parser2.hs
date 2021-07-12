-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser2 where

import DataStructures
import Helpers

-- implement stack like in script page 10 and ongoing

-- [TAtomExpr (T_VAR (Name "foo")),
-- TEQUAL,
-- T_IF,
-- TAtomExpr (T_INT 3),
-- TBinOp BO_EQUAL,
-- TAtomExpr (T_INT 3),
-- T_THEN,
-- TAtomExpr (T_INT 3),
-- T_ELSE,
-- TAtomExpr (T_INT 4),
-- TSEMICOL,
-- TAtomExpr (T_VAR (Name "bar")),
-- TEQUAL,
-- TAtomExpr (T_INT 4),
-- TSEMICOL,
-- T_MAIN,
-- TEQUAL,
-- TLPAREN,
-- TAtomExpr (T_VAR (Name "foo")),
-- TBinOp BO_PLUS,
-- TAtomExpr (T_VAR (Name "bar")),
-- TRPAREN,
-- TBinOp BO_MUL,
-- TAtomExpr (T_INT 4),
-- TBinOp BO_MINUS,
-- TAtomExpr (T_INT 134),
-- TSEMICOL]

-- first build only a valdidator that runs through when no issue is detected but that has no output
-- TODO: Find out: Am I actually just implementing a LL(0) Parser????

newtype InvalidSyntaxError = InvalidSyntaxError String
  deriving (Show)

--[T_MAIN,TEQUAL,TLPAREN,TAtomExpr (T_INT 2),TBinOp BO_PLUS,TAtomExpr (T_INT 10),TRPAREN,TBinOp BO_MUL,TAtomExpr (T_INT 4),TBinOp BO_MINUS,TAtomExpr (T_INT 134),TSEMICOL]

-- ATTEMPT 3

parseProgram :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseProgram = parseDefinitions CtxDef

parseDefinitions :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseDefinitions ctx (Left x) = Left x
parseDefinitions ctx (Right []) = Right []
parseDefinitions ctx (Right tokens) = parseDefinition ctx $ Right tokens

-- parseDefinitions (Right (TSEMICOL : rest)) = parseDefinitions $ Right rest -- Split between definitions happens here

parseDefinition :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseDefinition ctx (Left x) = Left x
parseDefinition ctx (Right []) = Right []
parseDefinition ctx (Right (T_MAIN : rest)) = parseEqualSign ctx $ Right rest -- since main is a special case it's treated here
parseDefinition ctx (Right all@(TAtomExpr (T_VAR (Name name)) : rest)) = parseVariables ctx $ Right all
parseDefinition ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseDefinition does not cover case: " ++ show other

-- Accepts only an equal sign
parseEqualSign :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseEqualSign ctx (Left x) = Left x
parseEqualSign ctx (Right []) = Right []
parseEqualSign ctx (Right all@(TEQUAL : rest)) = parseExpr ctx $ Right rest
parseEqualSign ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseEqualSign does not cover case: " ++ show other

-- accepts lists starting with a Variable or "="
parseVariables :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseVariables ctx (Left x) = Left x
parseVariables ctx (Right []) = Right []
parseVariables ctx (Right (TAtomExpr (T_VAR (Name name)) : rest)) = parseVariables ctx $ Right rest
parseVariables ctx (Right all@(TEQUAL : rest)) = parseEqualSign CtxDef $ Right all
parseVariables ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseVariables does not cover case: " ++ show other

parseExpr :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseExpr ctx (Left x) = Left x
parseExpr ctx (Right []) = Right []
parseExpr ctx (Right all@(T_LET : rest)) = parseLet ctx $ Right all
parseExpr ctx (Right all@(T_IN : rest)) = parseIn ctx $ Right all
parseExpr ctx (Right all@(T_IF : rest)) = parseIf ctx $ Right all -- TODO: continue here
parseExpr ctx (Right all@(T_ELSE : rest)) = parseElse ctx $ Right all
parseExpr ctx (Right all@(T_THEN : rest)) = parseThen ctx $ Right all
parseExpr CtxDef (Right (TSEMICOL : rest)) = parseDefinitions CtxDef $ Right rest
parseExpr CtxLocalDef (Right (TSEMICOL : rest)) = parseLocalDefinitions CtxLocalDef $ Right rest
parseExpr ctx (Right (TAtomExpr _ : rest)) = parseExpr ctx $ Right rest
parseExpr ctx (Right (TLPAREN : rest)) = parseExpr ctx $ Right rest
parseExpr ctx (Right (TRPAREN : rest)) = parseExpr ctx $ Right rest
parseExpr ctx other = parseExpr1 ctx other
-- parseExpr ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseExpr does not cover case: " ++ show other ++ " in context " ++ show ctx
-- think here

parseExpr1 :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseExpr1 ctx (Left x) = Left x
parseExpr1 ctx (Right []) = Right []
parseExpr1 ctx (Left x) = Left x
parseExpr1 ctx (Right []) = Right []
parseExpr1 ctx (Right all@(T_LET : rest)) = parseLet ctx $ Right all
parseExpr1 ctx (Right all@(T_IN : rest)) = parseIn ctx $ Right all
parseExpr1 ctx (Right all@(T_IF : rest)) = parseIf ctx $ Right all -- TODO: continue here
parseExpr1 ctx (Right all@(T_ELSE : rest)) = parseElse ctx $ Right all
parseExpr1 ctx (Right all@(T_THEN : rest)) = parseThen ctx $ Right all
parseExpr1 CtxDef (Right (TSEMICOL : rest)) = parseDefinitions CtxDef $ Right rest
parseExpr1 CtxLocalDef (Right (TSEMICOL : rest)) = parseLocalDefinitions CtxLocalDef $ Right rest
parseExpr1 ctx (Right (TAtomExpr _ : rest)) = parseExpr ctx $ Right rest
parseExpr1 ctx (Right (TLPAREN : rest)) = parseExpr ctx $ Right rest
parseExpr1 ctx (Right (TRPAREN : rest)) = parseExpr ctx $ Right rest
parseExpr1 ctx other = parseExpr1 ctx other
-- parseExpr1 ctx (Right all@()) = continue here !! 
-- parseExpr1 ctx (Right all@()) = 


-- parseExpr ctx (Right all@(TUniOp uni : rest)) = parseUniOp ctx $ Right all
-- parseExpr ctx (Right all@(TAtomExpr atom : rest)) = parseExpr ctx $ parseAtomicExpr ctx $ Right (TAtomExpr atom : rest) -- look into
-- parseExpr ctx (Right all@(TLPAREN : rest)) = parseParen ctx $ Right all
-- parseExpr ctx (Right all@(TBinOp bin : rest)) = parseBinOp ctx $ Right all
-- parseExpr ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseExpr does not cover case: " ++ show other ++ " in context " ++ show ctx

-- we leave out "Ausdruck Ausdruck" for now
--parseExpr (Right (TRPAREN:rest)) = Right rest
--parseExpr (Right all@(TBinOp bin:rest)) = parseExpr $ Right rest
--parseExpr (Right (TSEMICOL:rest)) = Right (TSEMICOL:rest)

-- we know that parenthesis have to close at some point
parseParen :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseParen ctx (Left x) = Left x
parseParen ctx (Right []) = Right []
parseParen ctx (Right (TLPAREN : rest)) = parseExpr ctx $ Right rest
parseParen ctx (Right (TRPAREN : rest)) = Right rest
parseParen ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseParen does not cover case: " ++ show other

parseLet :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseLet ctx (Left x) = Left x
parseLet ctx (Right []) = Right []
parseLet ctx (Right (T_LET : rest)) = parseLocalDefinitions ctx $ Right rest -- maybe another abstraction here
parseLet ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseLet does not cover case: " ++ show other

parseIn :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseIn ctx (Left x) = Left x
parseIn ctx (Right []) = Right []
parseIn ctx (Right (T_IN : rest)) = parseExpr ctx $ Right rest
parseIn ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseIn does not cover case: " ++ show other

parseIf :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseIf ctx (Left x) = Left x
parseIf ctx (Right []) = Right []
parseIf ctx (Right (T_IF : rest)) = parseExpr CtxOther $ Right rest
parseIf ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseIf does not cover case: " ++ show other

parseElse :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseElse ctx (Left x) = Left x
parseElse ctx (Right []) = Right []
parseElse ctx (Right (T_ELSE : rest)) = parseExpr CtxOther $ Right rest
parseElse ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseElse does not cover case: " ++ show other

parseThen :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseThen ctx (Left x) = Left x
parseThen ctx (Right []) = Right []
parseThen ctx (Right (T_THEN : rest)) = parseExpr ctx $ Right rest
parseThen ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseThen does not cover case: " ++ show other

parseLocalDefinitions :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseLocalDefinitions ctx (Left x) = Left x
parseLocalDefinitions ctx (Right []) = Right []
parseLocalDefinitions ctx (Right all@(TAtomExpr (T_VAR (Name name)) : rest)) = parseLocalDefinition ctx $ Right all
parseLocalDefinitions ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseLocalDefinitions does not cover case: " ++ show other

parseLocalDefinition :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseLocalDefinition ctx (Left x) = Left x
parseLocalDefinition ctx (Right []) = Right []
parseLocalDefinition ctx (Right (TAtomExpr (T_VAR (Name name)) : rest)) = parseEqualSign CtxLocalDef $ Right rest
parseLocalDefinition ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseLocalDefinition does not cover case: " ++ show other

parseBinOp :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseBinOp ctx (Left x) = Left x
parseBinOp ctx (Right []) = Right []
parseBinOp ctx (Right (TBinOp BO_AND : rest)) = Right rest
parseBinOp ctx (Right (TBinOp BO_OR : rest)) = Right rest
parseBinOp ctx (Right (TBinOp BO_EQUAL : rest)) = Right rest
parseBinOp ctx (Right (TBinOp BO_SMALLER : rest)) = Right rest
parseBinOp ctx (Right (TBinOp BO_PLUS : rest)) = Right rest
parseBinOp ctx (Right (TBinOp BO_MINUS : rest)) = Right rest
parseBinOp ctx (Right (TBinOp BO_MUL : rest)) = Right rest
parseBinOp ctx (Right (TBinOp BO_DIV : rest)) = Right rest
parseBinOp ctx other = Left $ InvalidSyntaxError $ show other
parseBinOp ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseBinOp does not cover case: " ++ show other

parseUniOp :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseUniOp ctx (Left x) = Left x
parseUniOp ctx (Right []) = Right []
parseUniOp ctx (Right (TUniOp UO_MINUS : rest)) = Right rest
parseUniOp ctx (Right (TUniOp UO_NOT : rest)) = Right rest
parseUniOp ctx other = Left $ InvalidSyntaxError $ show other
parseUniOp ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseUniOp does not cover case: " ++ show other

parseAtomicExpr :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseAtomicExpr ctx (Left x) = Left x
parseAtomicExpr ctx (Right []) = Right []
parseAtomicExpr ctx (Right (TAtomExpr (T_VAR (Name name)) : rest)) = parseVariable ctx $ Right (TAtomExpr (T_VAR (Name name)) : rest)
parseAtomicExpr ctx (Right (TAtomExpr (T_INT x) : rest)) = Right rest
parseAtomicExpr ctx (Right (TAtomExpr (T_BOOL bool) : rest)) = Right rest
parseAtomicExpr ctx other = Left $ InvalidSyntaxError $ show other
parseAtomicExpr ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseAtomicExpr does not cover case: " ++ show other

parseVariable :: Context -> Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseVariable ctx (Left x) = Left x
parseVariable ctx (Right []) = Right []
parseVariable ctx (Right (TAtomExpr (T_VAR (Name name)) : rest)) = Right rest
parseVariable ctx other = Left $ InvalidSyntaxError $ show other
parseVariable ctx (Right (other : rest)) = Left $ InvalidSyntaxError $ "parseVariable does not cover case: " ++ show other
