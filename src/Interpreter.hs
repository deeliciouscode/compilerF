module Interpreter where

import DataStructures
import Lexer
import Parser
import Data.Text
import Relude

-- F Spezifikationen (Skript Seite 61):
-- Lokale Definitionen sind in F möglich, jedoch eingeschränkt auf Definitionen von Ausdrücken ohne Parameter (Wertedefinitionen).
-- F ermöglicht nicht die Definition von Aufzählungs- und strukturierten Typen. Folglich bietet F die Mustererkennung nicht an.
-- Die λ-Abstraktion fehlt in F und F kann keine Funktionen höherer Ordnung behandeln.
-- Die Übersetzung von F implementiert die Endrekursion nicht.

-- Jedes Program sieht so aus: main = Ausdruck
-- main darf nur ein mal existieren und ist der einzige auszuwertende Ausdruck.

-- the output of each step is the input to the next one

-- inputString = "foo = if 3 == 3 then 3 else 4; bar = 4; main = (foo + bar) * 4 - 134;"
-- inputString = "foo = 3; bar = 4; main = (foo + bar) * 4 - 134;"
-- inputString = "main = (2 + 10) * 4 - 134;"
-- inputString = "main = (2 + 10) * 4;"
inputString = "let a = True in if a then 69 else 420;"
-- NOTE: Up untill now we can only parse Expressions.

dynamic :: IO ()
dynamic = do 
    Relude.putStrLn "Type an expression to parse: "
    string <- Relude.getLine
    let lexed = genListOfTokens $ unpack string in
        let parsed = parseExpression lexed in 
            Relude.print parsed

static :: IO ()
static = do
  -- lexical analysis (Lexer)
  let listOfTokens = genListOfTokens inputString
  Relude.print listOfTokens
  -- syntactic analysis (Parser)
  let err = parseExpression listOfTokens
  Relude.print err

-- semantic Analysis

-- code generation

-- code optimization (not necessary)

-- assembly (Assembler)




-----------------------------------------------------------------------------

-- let a=3; b=2; c=4 in a+b+c; 

-- (Just 
--    (LetIn 
--      (LocDefs 
--        (LocDef (Name "a") (Int 3)) 
--        (RLocDefs 
--          (LocDefs 
--            (LocDef (Name "b") (Int 2)) 
--            (RLocDefs 
--              (LocDefs 
--                (LocDef (Name "c") (Int 4)) 
--                LDeps))))) 
--      (Plus 
--        (Variable "a") 
--        (Plus 
--          (Variable "b") 
--          (Variable "c"))))


-----------------------------------------------------------------------------




-- Basic Structure of a Parser

-- a :: [Token] -> (Maybe AstA, [Token])
-- a tokensRest0@(B : _) =
--   case b tokensRest0 of
--     (Nothing, tokensRest1) -> (Nothing, tokensRest1)
--     (Just astB, tokensRest1) ->
--       case c tokensRest1 of
--         (Nothing, tokensRest2) -> (Nothing, tokensRest2)
--         (Just astC, tokensRest2) -> (Just $ f astB astC, tokensRest2)
-- a tokensRest0@(D : _) =
--   case d tokensRest0 of
--     (Nothing, tokensRest1) -> (Nothing, tokensRest1)
--     (Just astD, tokensRest1) ->
--       case e tokensRest1 of
--         (Nothing, tokensRest2) -> (Nothing, tokensRest2)
--         (Just astE, tokensRest2) -> (Just $ g astD astE, tokensRest2)
-- a tokensRest0 = (Nothing, tokensRest0)

-- f :: AstB -> AstC -> AstA
-- g :: AstD -> AstE -> AstA
-- b :: [Token] -> (Maybe AstB, [Token])
-- c :: [Token] -> (Maybe AstC, [Token])
-- d :: [Token] -> (Maybe AstD, [Token])
-- e :: [Token] -> (Maybe AstE, [Token])
