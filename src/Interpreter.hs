module Interpreter where
import DataStructures
import Lexer
import Parser

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
inputString = "main = (2 + 10) * 4;"


main :: IO ()
main = do
    -- lexical analysis (Lexer)
    let listOfTokens = genListOfTokens inputString
    print listOfTokens
    -- syntactic analysis (Parser)
    let err = parseProgram $ Right listOfTokens
    print err

-- semantic Analysis 

-- code generation

-- code optimization (not necessary)

-- assembly (Assembler)

