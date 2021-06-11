module Parser where
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

newtype InvalidSyntaxError = InvalidSyntaxError String 
                deriving (Show)

--[T_MAIN,TEQUAL,TLPAREN,TAtomExpr (T_INT 2),TBinOp BO_PLUS,TAtomExpr (T_INT 10),TRPAREN,TBinOp BO_MUL,TAtomExpr (T_INT 4),TBinOp BO_MINUS,TAtomExpr (T_INT 134),TSEMICOL]


-- ATTEMPT 3

parseProgram :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseProgram = parseDefinitions


parseDefinitions :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseDefinitions (Left x) = Left x
parseDefinitions (Right []) = Right [] 
parseDefinitions (Right (TSEMICOL:rest)) = parseDefinitions $ Right rest  
parseDefinitions (Right tokens) = parseDefinitions $ parseDefinition $ Right tokens


parseDefinition :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseDefinition (Left x) = Left x   
parseDefinition (Right all@(TAtomExpr (T_VAR (Name name)):rest)) = parseDefinition $ parseVariables $ Right all
parseDefinition (Right (TEQUAL:rest)) = parseDefinition $ parseExpr $ Right rest
parseDefinition (Right (T_MAIN:rest)) = parseDefinition $ Right rest
parseDefinition (Right all@(TSEMICOL:rest)) = Right all
parseDefinition (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseDefinition does not cover case: " ++ show other


parseVariables :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseVariables (Left x) = Left x
parseVariables (Right (TAtomExpr (T_VAR (Name name)):rest)) = parseVariables $ Right rest
parseVariables (Right all@(TEQUAL:rest)) = Right all
parseVariables (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseVariables does not cover case: " ++ show other


parseExpr :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseExpr (Left x) = Left x 
parseExpr (Right all@(T_LET:rest)) = parseLet $ Right all
parseExpr (Right all@(T_IF:rest)) = parseIfElse $ Right all -- TODO: continue here
parseExpr (Right all@(TUniOp uni:rest)) = parseUniOp $ Right all
parseExpr (Right all@(TAtomExpr atom:rest)) = parseExpr $ parseAtomicExpr $ Right (TAtomExpr atom:rest) -- look into
parseExpr (Right all@(TLPAREN:rest)) = parseParen $ Right all
parseExpr (Right all@(TBinOp bin:rest)) = parseBinOp $ Right all
parseExpr (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseExpr does not cover case: " ++ show other
-- we leave out "Ausdruck Ausdruck" for now
--parseExpr (Right (TRPAREN:rest)) = Right rest
--parseExpr (Right all@(TBinOp bin:rest)) = parseExpr $ Right rest
--parseExpr (Right (TSEMICOL:rest)) = Right (TSEMICOL:rest) 

-- we know that parenthesis have to close at some point 
parseParen :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseParen (Right (TLPAREN:rest)) = parseParen $ Right rest
parseParen (Right (TRPAREN:rest)) = Right rest
parseParen (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseParenth does not cover case: " ++ show other

parseIfElse :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseIfElse (Left x) = Left x
parseIfElse (Right (T_IF:rest)) = parseExpr $ Right rest
parseIfElse (Right (T_ELSE:rest)) = parseExpr $ Right rest
parseIfElse (Right (T_THEN:rest)) = parseExpr $ Right rest
parseIfElse (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseIfElse does not cover case: " ++ show other
parseIfElse expr = parseExpr expr

parseLet :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseLet (Left x) = Left x 
parseLet (Right (T_LET:rest)) = parseLet $ Right rest -- maybe another abstraction here
parseLet (Right all@(TAtomExpr (T_VAR (Name name)):rest)) = parseIn $ parseLocalDefinitions $ Right all 
parseLet (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseLet does not cover case: " ++ show other

parseIn :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseIn (Left x) = Left x 
parseIn (Right (T_IN:rest)) = parseExpr $ Right rest 
parseIn (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseIn does not cover case: " ++ show other

parseLocalDefinitions :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseLocalDefinitions (Left x) = Left x 
parseLocalDefinitions (Right (TAtomExpr (T_VAR (Name name)):rest)) = parseLocalDefinition (Right (TAtomExpr (T_VAR (Name name)):rest))
parseLocalDefinitions (Right (TSEMICOL:rest)) = parseLocalDefinitions $ Right rest
parseLocalDefinitions (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseLocalDefinitions does not cover case: " ++ show other

parseLocalDefinition :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseLocalDefinition (Left x) = Left x 
parseLocalDefinition (Right (TSEMICOL:rest)) = Right (TSEMICOL:rest)
parseLocalDefinition (Right (TAtomExpr (T_VAR (Name name)):rest)) = parseLocalDefinition $ parseEqualSign $ Right rest
parseLocalDefinition expr = parseLocalDefinition $ parseExpr expr
parseLocalDefinition (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseLocalDefinition does not cover case: " ++ show other

parseEqualSign :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseEqualSign (Left x) = Left x 
parseEqualSign (Right (TEQUAL:rest)) = Right rest
parseEqualSign other = Left $ InvalidSyntaxError $ show other
parseEqualSign (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseEqualSign does not cover case: " ++ show other

parseBinOp :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseBinOp (Left x) = Left x 
parseBinOp (Right (TBinOp BO_AND:rest)) = Right rest
parseBinOp (Right (TBinOp BO_OR:rest)) = Right rest
parseBinOp (Right (TBinOp BO_EQUAL:rest)) = Right rest
parseBinOp (Right (TBinOp BO_SMALLER:rest)) = Right rest
parseBinOp (Right (TBinOp BO_PLUS:rest)) = Right rest
parseBinOp (Right (TBinOp BO_MINUS:rest)) = Right rest
parseBinOp (Right (TBinOp BO_MUL:rest)) = Right rest
parseBinOp (Right (TBinOp BO_DIV:rest)) = Right rest
parseBinOp other = Left $ InvalidSyntaxError $ show other
parseBinOp (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseBinOp does not cover case: " ++ show other

parseUniOp :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseUniOp (Left x) = Left x 
parseUniOp (Right (TUniOp UO_MINUS:rest)) = Right rest
parseUniOp (Right (TUniOp UO_NOT:rest)) = Right rest
parseUniOp other = Left $ InvalidSyntaxError $ show other
parseUniOp (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseUniOp does not cover case: " ++ show other

parseAtomicExpr :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseAtomicExpr (Left x) = Left x 
parseAtomicExpr (Right (TAtomExpr (T_VAR (Name name)):rest)) = parseVariable $ Right (TAtomExpr (T_VAR (Name name)):rest)
parseAtomicExpr (Right (TAtomExpr (T_INT x):rest)) = Right rest
parseAtomicExpr (Right (TAtomExpr (T_BOOL bool):rest)) = Right rest
parseAtomicExpr other = Left $ InvalidSyntaxError $ show other
parseAtomicExpr (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseAtomicExpr does not cover case: " ++ show other

parseVariable :: Either InvalidSyntaxError [Token] -> Either InvalidSyntaxError [Token]
parseVariable (Left x) = Left x 
parseVariable (Right (TAtomExpr (T_VAR (Name name)):rest)) = Right rest 
parseVariable other = Left $ InvalidSyntaxError $ show other
parseVariable (Right (other:rest)) = Left $ InvalidSyntaxError $ "parseVariable does not cover case: " ++ show other











-- ATTEMPT 2

-- validateProgram :: Maybe [Token] -> Maybe [Token]
-- validateProgram Nothing  = Nothing 
-- validateProgram  (Just []) = Just [] 
-- validateProgram (Just tokens) = validateProgram $ validateDefinition $ Just tokens

-- validateDefinition :: Maybe [Token] -> Maybe [Token]
-- validateDefinition Nothing = Nothing   
-- validateDefinition (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateDefinition $ Just rest
-- validateDefinition (Just (T_MAIN:rest)) = validateDefinition $ Just rest
-- validateDefinition (Just (TEQUAL:rest)) = validateDefinition $ validateExpr $ Just rest
-- validateDefinition (Just (TSEMICOL:rest)) = Just rest 

-- validateExpr :: Maybe [Token] -> Maybe [Token]
-- validateExpr Nothing = Nothing 
-- validateExpr (Just (TSEMICOL:rest)) = Just (TSEMICOL:rest) 
-- validateExpr (Just (T_LET:rest)) = validateExpr $ validateLet $ Just (T_LET:rest)
-- validateExpr (Just (T_IF:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_ELSE:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_THEN:rest)) = validateExpr $ Just rest
-- validateExpr (Just (TUniOp uni:rest)) = validateExpr $ validateUniOp $ Just (TUniOp uni:rest)
-- validateExpr (Just (TAtomExpr atom:rest)) = validateExpr $ validateAtomicExpr $ Just (TAtomExpr atom:rest)
-- validateExpr (Just (TBinOp bin:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_MAIN:rest)) = validateExpr $ Just rest
-- validateExpr (Just (TLPAREN:rest)) = validateParenth $ Just rest

-- -- we know that parenthesis have to close at some point 
-- validateParenth :: Maybe [Token] -> Maybe [Token]
-- validateParenth (Just (TLPAREN:rest)) = validateParenth $ Just rest
-- validateParenth (Just (TRPAREN:rest)) = Just rest

-- validateIf :: Maybe [Token] -> Maybe [Token]
-- validateIf Nothing = Nothing
-- validateIf (Just (T_IF:rest)) = validateExpr $ Just rest
-- validateIf (Just (T_ELSE:rest)) = validateExpr $ Just rest
-- validateIf (Just (T_THEN:rest)) = validateExpr $ Just rest
-- validateIf expr = validateExpr expr

-- validateLet :: Maybe [Token] -> Maybe [Token]
-- validateLet Nothing = Nothing 
-- validateLet (Just (T_LET:rest)) = validateLet $ Just rest 
-- validateLet (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinitions $ Just (TAtomExpr (T_VAR (Name name)):rest) 

-- validateLocalDefinitions :: Maybe [Token] -> Maybe [Token]
-- validateLocalDefinitions Nothing = Nothing 
-- validateLocalDefinitions (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinition (Just (TAtomExpr (T_VAR (Name name)):rest))
-- validateLocalDefinitions (Just (TSEMICOL:rest)) = validateLocalDefinitions $ Just rest

-- validateLocalDefinition :: Maybe [Token] -> Maybe [Token]
-- validateLocalDefinition Nothing = Nothing 
-- validateLocalDefinition (Just (TSEMICOL:rest)) = Just (TSEMICOL:rest)
-- validateLocalDefinition (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinition $ validateEqualSign $ Just rest
-- validateLocalDefinition expr = validateLocalDefinition $ validateExpr expr

-- validateEqualSign :: Maybe [Token] -> Maybe [Token]
-- validateEqualSign Nothing = Nothing 
-- validateEqualSign (Just (TEQUAL:rest)) = Just rest
-- validateEqualSign other = Nothing 

-- validateBinOp :: Maybe [Token] -> Maybe [Token]
-- validateBinOp Nothing = Nothing 
-- validateBinOp (Just (TBinOp BO_AND:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_OR:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_EQUAL:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_SMALLER:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_PLUS:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_MINUS:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_MUL:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_DIV:rest)) = Just rest
-- validateBinOp other = Nothing

-- validateUniOp :: Maybe [Token] -> Maybe [Token]
-- validateUniOp Nothing = Nothing 
-- validateUniOp (Just (TUniOp UO_MINUS:rest)) = Just rest
-- validateUniOp (Just (TUniOp UO_NOT:rest)) = Just rest
-- validateUniOp other = Nothing

-- validateAtomicExpr :: Maybe [Token] -> Maybe [Token]
-- validateAtomicExpr Nothing = Nothing 
-- validateAtomicExpr (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateVariable $ Just (TAtomExpr (T_VAR (Name name)):rest)
-- validateAtomicExpr (Just (TAtomExpr (T_INT x):rest)) = Just rest
-- validateAtomicExpr (Just (TAtomExpr (T_BOOL bool):rest)) = Just rest
-- validateAtomicExpr other = Nothing 

-- validateVariable :: Maybe [Token] -> Maybe [Token]
-- validateVariable Nothing = Nothing 
-- validateVariable (Just (TAtomExpr (T_VAR (Name name)):rest)) = Just rest 
-- validateVariable other = Nothing 




-- ATTEMPT 1

-- Not functional, will be refactored as a next step

-- parseTree :: [Token] -> SyntaxTree Token
-- parseTree [] = Leaf TNULL
-- parseTree tokens = makeTree (slice 0 i tokens) (tokens!!i) (slice (i+1) (length tokens) tokens)
--                 where i = findRootIndex tokens

-- makeTree :: [Token] -> Token -> [Token] -> SyntaxTree Token
-- makeTree left binOp right = Node (makeBranch binOp left) (parseTree right)

-- makeBranch :: Token -> [Token] -> SyntaxTree Token
-- makeBranch (TBinOp op) tokens = Node (Leaf (TBinOp op)) (parseTree tokens) 

-- findRootIndex :: [Token] -> Int
-- findRootIndex tokens = findRootIndex' [] tokens 0 

-- findRootIndex' :: [Token] -> [Token] -> Int -> Int
-- findRootIndex' [] (x:xs) i = findRootIndex' [x] xs i+1
-- findRootIndex' left right@(y:ys) i
--                 | foundCenter left right = i
--                 | otherwise = findRootIndex' (y:left) ys i+1
-- findRootIndex' _ _ _ = 0

-- foundCenter :: [Token] -> [Token] -> Bool 
-- foundCenter left right 
--                 | count TLPAREN left == count TRPAREN left 
--                         && count TLPAREN right == count TRPAREN right = True
--                 | count TLPAREN left == count TRPAREN left + 1 
--                         && count TLPAREN right + 1 == count TRPAREN right = True
--                 | otherwise = False



-- makePrefix :: [Token] -> [Token]
-- makePrefix = makePrefix' []

-- makePrefix' :: [Token] -> [Token] -> [Token]
-- makePrefix' left [] = left 
-- makePrefix' (TLPAREN:left) (TBinOp op:xs) = TLPAREN : TBinOp op : left ++ makePrefix' [] xs   
-- makePrefix' left (TBinOp op:xs) = TBinOp op : left ++ makePrefix' [] xs   
-- makePrefix' left (x:xs) = makePrefix' (left ++ [x]) xs 

-- containerize :: [Token] -> [Token]
-- containerize = containerize' [] []

-- containerize' :: [Token] -> [Token] -> [Token] -> [Token]
-- containerize' new left [] = new ++ left
-- containerize' new left (x:right) 
--                         | x /= TLPAREN && x /= TRPAREN && count TLPAREN left == 0 
--                                 = containerize' (new ++ [x]) left right  
                        
--                         | x == TLPAREN = containerize' new (left ++ [x]) right
--                         | x == TRPAREN && count TLPAREN left == count TRPAREN left + 1 
--                                 = containerize' (new ++ [TCONTAINER (left ++ [x])]) [] right
                        
--                         | otherwise = containerize' new (left ++ [x]) right

-- validateProgram :: Maybe [Token] -> Maybe [Token]
-- validateProgram Nothing  = Nothing 
-- validateProgram  (Just []) = Just [] 
-- validateProgram (Just tokens) = validateProgram $ validateDefinition $ Just tokens

-- validateDefinition :: Maybe [Token] -> Maybe [Token]
-- validateDefinition Nothing = Nothing   
-- validateDefinition (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateDefinition $ Just rest
-- validateDefinition (Just (T_MAIN:rest)) = validateDefinition $ Just rest
-- validateDefinition (Just (TEQUAL:rest)) = validateDefinition $ validateExpr $ Just rest
-- validateDefinition (Just (TSEMICOL:rest)) = Just rest 

-- validateExpr :: Maybe [Token] -> Maybe [Token]
-- validateExpr Nothing = Nothing 
-- validateExpr (Just (TSEMICOL:validateProgram :: Maybe [Token] -> Maybe [Token]
-- validateProgram Nothing  = Nothing 
-- validateProgram  (Just []) = Just [] 
-- validateProgram (Just tokens) = validateProgram $ validateDefinition $ Just tokens

-- validateDefinition :: Maybe [Token] -> Maybe [Token]
-- validateDefinition Nothing = Nothing   
-- validateDefinition (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateDefinition $ Just rest
-- validateDefinition (Just (T_MAIN:rest)) = validateDefinition $ Just rest
-- validateDefinition (Just (TEQUAL:rest)) = validateDefinition $ validateExpr $ Just rest
-- validateDefinition (Just (TSEMICOL:rest)) = Just rest 

-- validateExpr :: Maybe [Token] -> Maybe [Token]
-- validateExpr Nothing = Nothing 
-- validateExpr (Just (TSEMICOL:rest)) = Just (TSEMICOL:rest) 
-- validateExpr (Just (T_LET:rest)) = validateExpr $ validateLet $ Just (T_LET:rest)
-- validateExpr (Just (T_IF:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_ELSE:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_THEN:rest)) = validateExpr $ Just rest
-- validateExpr (Just (TUniOp uni:rest)) = validateExpr $ validateUniOp $ Just (TUniOp uni:rest)
-- validateExpr (Just (TAtomExpr atom:rest)) = validateExpr $ validateAtomicExpr $ Just (TAtomExpr atom:rest)
-- validateExpr (Just (TBinOp bin:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_MAIN:rest)) = validateExpr $ Just rest
-- validateExpr (Just (TLPAREN:rest)) = validateParenth $ Just rest

-- -- we know that parenthesis have to close at some point 
-- validateParenth :: Maybe [Token] -> Maybe [Token]
-- validateParenth (Just (TLPAREN:rest)) = validateParenth $ Just rest
-- validateParenth (Just (TRPAREN:rest)) = Just rest

-- validateIf :: Maybe [Token] -> Maybe [Token]
-- validateIf Nothing = Nothing
-- validateIf (Just (T_IF:rest)) = validateExpr $ Just rest
-- validateIf (Just (T_ELSE:rest)) = validateExpr $ Just rest
-- validateIf (Just (T_THEN:rest)) = validateExpr $ Just rest
-- validateIf expr = validateExpr expr

-- validateLet :: Maybe [Token] -> Maybe [Token]
-- validateLet Nothing = Nothing 
-- validateLet (Just (T_LET:rest)) = validateLet $ Just rest 
-- validateLet (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinitions $ Just (TAtomExpr (T_VAR (Name name)):rest) 

-- validateLocalDefinitions :: Maybe [Token] -> Maybe [Token]
-- validateLocalDefinitions Nothing = Nothing 
-- validateLocalDefinitions (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinition (Just (TAtomExpr (T_VAR (Name name)):rest))
-- validateLocalDefinitions (Just (TSEMICOL:rest)) = validateLocalDefinitions $ Just rest

-- validateLocalDefinition :: Maybe [Token] -> Maybe [Token]
-- validateLocalDefinition Nothing = Nothing 
-- validateLocalDefinition (Just (TSEMICOL:rest)) = Just (TSEMICOL:rest)
-- validateLocalDefinition (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinition $ validateEqualSign $ Just rest
-- validateLocalDefinition expr = validateLocalDefinition $ validateExpr expr

-- validateEqualSign :: Maybe [Token] -> Maybe [Token]
-- validateEqualSign Nothing = Nothing 
-- validateEqualSign (Just (TEQUAL:rest)) = Just rest
-- validateEqualSign other = Nothing 

-- validateBinOp :: Maybe [Token] -> Maybe [Token]
-- validateBinOp Nothing = Nothing 
-- validateBinOp (Just (TBinOp BO_AND:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_OR:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_EQUAL:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_SMALLER:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_PLUS:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_MINUS:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_MUL:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_DIV:rest)) = Just rest
-- validateBinOp other = Nothing

-- validateUniOp :: Maybe [Token] -> Maybe [Token]
-- validateUniOp Nothing = Nothing 
-- validateUniOp (Just (TUniOp UO_MINUS:rest)) = Just rest
-- validateUniOp (Just (TUniOp UO_NOT:rest)) = Just rest
-- validateUniOp other = Nothing

-- validateAtomicExpr :: Maybe [Token] -> Maybe [Token]
-- validateAtomicExpr Nothing = Nothing 
-- validateAtomicExpr (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateVariable $ Just (TAtomExpr (T_VAR (Name name)):rest)
-- validateAtomicExpr (Just (TAtomExpr (T_INT x):rest)) = Just rest
-- validateAtomicExpr (Just (TAtomExpr (T_BOOL bool):rest)) = Just rest
-- validateAtomicExpr other = Nothing 

-- validateVariable :: Maybe [Token] -> Maybe [Token]
-- validateVariable Nothing = Nothing 
-- validateVariable (Just (TAtomExpr (T_VAR (Name name)):rest)) = Just rest 
-- validateVariable other = Nothing rest)) = Just (TSEMICOL:rest) 
-- validateExpr (Just (T_LET:rest)) = validateExpr $ validateLet $ Just (T_LET:rest)
-- validateExpr (Just (T_IF:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_ELSE:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_THEN:rest)) = validateExpr $ Just rest
-- validateExpr (Just (TUniOp uni:rest)) = validateExpr $ validateUniOp $ Just (TUniOp uni:rest)
-- validateExpr (Just (TAtomExpr atom:rest)) = validateExpr $ validateAtomicExpr $ Just (TAtomExpr atom:rest)
-- validateExpr (Just (TBinOp bin:rest)) = validateExpr $ Just rest
-- validateExpr (Just (T_MAIN:rest)) = validateExpr $ Just rest
-- validateExpr (Just (TLPAREN:rest)) = validateParenth $ Just rest

-- -- we know that parenthesis have to close at some point 
-- validateParenth :: Maybe [Token] -> Maybe [Token]
-- validateParenth (Just (TLPAREN:rest)) = validateParenth $ Just rest
-- validateParenth (Just (TRPAREN:rest)) = Just rest

-- validateIf :: Maybe [Token] -> Maybe [Token]
-- validateIf Nothing = Nothing
-- validateIf (Just (T_IF:rest)) = validateExpr $ Just rest
-- validateIf (Just (T_ELSE:rest)) = validateExpr $ Just rest
-- validateIf (Just (T_THEN:rest)) = validateExpr $ Just rest
-- validateIf expr = validateExpr expr

-- validateLet :: Maybe [Token] -> Maybe [Token]
-- validateLet Nothing = Nothing 
-- validateLet (Just (T_LET:rest)) = validateLet $ Just rest 
-- validateLet (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinitions $ Just (TAtomExpr (T_VAR (Name name)):rest) 

-- validateLocalDefinitions :: Maybe [Token] -> Maybe [Token]
-- validateLocalDefinitions Nothing = Nothing 
-- validateLocalDefinitions (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinition (Just (TAtomExpr (T_VAR (Name name)):rest))
-- validateLocalDefinitions (Just (TSEMICOL:rest)) = validateLocalDefinitions $ Just rest

-- validateLocalDefinition :: Maybe [Token] -> Maybe [Token]
-- validateLocalDefinition Nothing = Nothing 
-- validateLocalDefinition (Just (TSEMICOL:rest)) = Just (TSEMICOL:rest)
-- validateLocalDefinition (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateLocalDefinition $ validateEqualSign $ Just rest
-- validateLocalDefinition expr = validateLocalDefinition $ validateExpr expr

-- validateEqualSign :: Maybe [Token] -> Maybe [Token]
-- validateEqualSign Nothing = Nothing 
-- validateEqualSign (Just (TEQUAL:rest)) = Just rest
-- validateEqualSign other = Nothing 

-- validateBinOp :: Maybe [Token] -> Maybe [Token]
-- validateBinOp Nothing = Nothing 
-- validateBinOp (Just (TBinOp BO_AND:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_OR:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_EQUAL:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_SMALLER:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_PLUS:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_MINUS:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_MUL:rest)) = Just rest
-- validateBinOp (Just (TBinOp BO_DIV:rest)) = Just rest
-- validateBinOp other = Nothing

-- validateUniOp :: Maybe [Token] -> Maybe [Token]
-- validateUniOp Nothing = Nothing 
-- validateUniOp (Just (TUniOp UO_MINUS:rest)) = Just rest
-- validateUniOp (Just (TUniOp UO_NOT:rest)) = Just rest
-- validateUniOp other = Nothing

-- validateAtomicExpr :: Maybe [Token] -> Maybe [Token]
-- validateAtomicExpr Nothing = Nothing 
-- validateAtomicExpr (Just (TAtomExpr (T_VAR (Name name)):rest)) = validateVariable $ Just (TAtomExpr (T_VAR (Name name)):rest)
-- validateAtomicExpr (Just (TAtomExpr (T_INT x):rest)) = Just rest
-- validateAtomicExpr (Just (TAtomExpr (T_BOOL bool):rest)) = Just rest
-- validateAtomicExpr other = Nothing 

-- validateVariable :: Maybe [Token] -> Maybe [Token]
-- validateVariable Nothing = Nothing 
-- validateVariable (Just (TAtomExpr (T_VAR (Name name)):rest)) = Just rest 
-- validateVariable other = Nothing 

-- structured :: [Token] -> [Token]
-- structured = structured' []

-- structured' :: [Token] -> [Token] -> [Token]
-- structured' left right
--                 | last right == TRPAREN = right
--                 | last right == TLPAREN 


