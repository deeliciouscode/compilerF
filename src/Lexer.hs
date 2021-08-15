module Lexer where
import Text.Read
import Data.Maybe
import Constants
import DataStructures
import Helpers


-- FIX unär minus bug 
-- genListOfTokens "a = 1; b = -2; add a b = a + b; applied = add a b;" -> the -2 is not recognized

genListOfTokens :: String -> [Token]
genListOfTokens str = retokenize $ dialex str

-- groups tokens that belong to each other together  
retokenize :: [Token] -> [Token]
retokenize [] = []
retokenize (x:xs:xss)
                | x == TEQUAL && xs == TEQUAL = TBinOp BO_EQUAL : retokenize xss
                | x == TLPAREN && xs == TBinOp BO_MINUS = x : TUniOp UO_MINUS : retokenize xss
retokenize (x:xs) = x : retokenize xs

dialex :: String -> [Token]
dialex "" = []
dialex sent@(x:xs) 
                | x == ' ' = dialex xs
                | otherwise = tokenize (fst result) : dialex (snd result)
                where result = sliceToken "" sent


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
tokenize "main" = T_MAIN
tokenize "let" = T_LET 
tokenize "in" = T_IN
tokenize "if" = T_IF  
tokenize "then" = T_THEN 
tokenize "else" = T_ELSE 
tokenize other 
            | isJust (readMaybe other :: Maybe Bool) = TAtomExpr $ T_BOOL (read other)
            | isJust (readMaybe other :: Maybe Int) = TAtomExpr $ T_INT (read other)
tokenize var = TAtomExpr $ T_VAR $ Name var
-- tokenize _ = TNULL
