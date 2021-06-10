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

inputString = "foo = if 3 == 3 then 3 else 4; bar = 4; main = (foo + bar) * 4 - 134;"

-- lexical analysis (Lexer)
listOfTokens :: [Token]
listOfTokens = genListOfTokens inputString

-- syntactic analysis (Parser)

-- semantic Analysis 

-- code generation

-- code optimization (not necessary)

-- assembly (Assembler)

