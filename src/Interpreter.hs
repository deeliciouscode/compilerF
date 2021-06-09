module Interpreter where
import DataStructures
import Lexer
import Parser

-- the output of each step is the input to the next one

inputString = "(3 + 4) * 4 - 134"

-- lexical analysis (Lexer)
listOfTokens :: [Token]
listOfTokens = genListOfTokens inputString

-- syntactic analysis (Parser)

-- semantic Analysis 

-- code generation

-- code optimization (not necessary)

-- assembly (Assembler)

