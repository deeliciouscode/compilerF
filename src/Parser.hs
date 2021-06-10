module Parser where
import DataStructures
import Helpers


-- implement stack like in script page 10 and ongoing 

parse :: [Token] -> SyntaxTree Token 
parse _ = Leaf TNULL 






































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



-- structured :: [Token] -> [Token]
-- structured = structured' []

-- structured' :: [Token] -> [Token] -> [Token]
-- structured' left right
--                 | last right == TRPAREN = right
--                 | last right == TLPAREN 


