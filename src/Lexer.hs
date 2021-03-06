module Lexer where
import Text.Read
import Data.Maybe
import DataStructures


genListOfTokens :: String -> [Token]
genListOfTokens str = retokenize $ dialex $ removeNewLinesAndComments str

-- groups tokens that belong to each other together  
retokenize :: [Token] -> [Token]
retokenize [] = []
retokenize (x:xs:xss)
                | x == TEQUAL && xs == TEQUAL = TBinOp BO_EQUAL : retokenize xss
                | x == TLPAREN && xs == TBinOp BO_MINUS = x : TUniOp UO_MINUS : retokenize xss
                | x == TEQUAL && xs == TBinOp BO_MINUS = x : TUniOp UO_MINUS : retokenize xss
                | x == T_IN && xs == TBinOp BO_MINUS = x : TUniOp UO_MINUS : retokenize xss
                | x == T_IF && xs == TBinOp BO_MINUS = x : TUniOp UO_MINUS : retokenize xss
                | x == T_THEN && xs == TBinOp BO_MINUS = x : TUniOp UO_MINUS : retokenize xss
                | x == T_ELSE && xs == TBinOp BO_MINUS = x : TUniOp UO_MINUS : retokenize xss
retokenize (x:xs) = x : retokenize xs

dialex :: String -> [Token]
dialex "" = []
dialex sent@(x:xs) 
                | x == ' ' = dialex xs
                | otherwise = tokenize (fst result) : dialex (snd result)
                where result = sliceToken "" sent

stripWhiteSpace :: String -> String
stripWhiteSpace [] = []
stripWhiteSpace all@(x:xs)
                | x == ' ' = stripWhiteSpace xs
                | otherwise = all

doesNotStartWithDoubleDash :: String -> Bool
doesNotStartWithDoubleDash (head:second:rest)
                | head == '-' && second == '-' = False
                | otherwise = True
doesNotStartWithDoubleDash _ = True

isNotCommented :: String -> Bool
isNotCommented = doesNotStartWithDoubleDash . stripWhiteSpace

removeNewLinesAndComments :: String -> String 
removeNewLinesAndComments = concat . filter isNotCommented . lines 

sliceToken :: String -> String -> (String, String)
sliceToken word "" = (word, "")
sliceToken "" rest@(x:xs)
                | isSpecialChar x = (toStr x, xs)
sliceToken word rest@(x:xs)
                | isSpecialChar x = (word, rest)
                | x == ' ' = (word, xs)
                | otherwise = sliceToken (word ++ [x]) xs

-- "+-*/();=>&|"
tokenize :: String -> Token
tokenize "+" = TBinOp BO_PLUS
tokenize "-" = TBinOp BO_MINUS
tokenize "*" = TBinOp BO_MUL
tokenize "/" = TBinOp BO_DIV
tokenize "(" = TLPAREN
tokenize ")" = TRPAREN
tokenize ";" = TSEMICOL 
tokenize "=" = TEQUAL
tokenize "<" = TBinOp BO_SMALLER
tokenize "&" = TBinOp BO_AND
tokenize "|" = TBinOp BO_OR
tokenize "not" = TUniOp UO_NOT
tokenize "let" = T_LET 
tokenize "in" = T_IN
tokenize "if" = T_IF  
tokenize "then" = T_THEN 
tokenize "else" = T_ELSE 
tokenize other 
            | isJust (readMaybe other :: Maybe Bool) = TAtomExpr $ T_BOOL (read other)
            | isJust (readMaybe other :: Maybe Int) = TAtomExpr $ T_INT (read other)
tokenize var = TAtomExpr $ T_VAR $ Name var


--------------------------- HELPER & CONSTANTS ---------------------------

toStr :: Char -> String 
toStr x = [x]

isSpecialChar :: Char -> Bool
isSpecialChar x = isPartOfList x specialChars

isSpecialWord :: String -> Bool
isSpecialWord x = isPartOfList x specialWords

isPartOfList :: Eq a => a -> [a] -> Bool
isPartOfList _ [] = False
isPartOfList item (x:xs)
                | item == x = True
                | otherwise = isPartOfList item xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs) 

specialChars :: [Char]
specialChars = "+-*/();=><&|"

specialWords :: [[Char]]
specialWords = ["if", "then", "else", "not", "let", "in"]

