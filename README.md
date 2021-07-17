# ImProg_language_f

## Definitions

### Grammar, context-free Grammar, regular Grammar

- Σ = Alphabet
- Σ∗ = Menge aller Wörter aus Σ
- Σ+ = Σ∗ \ {\eps}

- Grammar is a 4-Tupel G = (V, Σ, P, S)

- V ist ein endliche Menge. Ihre Elemente heißen Variablen oder Nichtterminalsymbole von G.

- Σ ist eine nichtleere endliche Menge mit V ∩ Σ = ∅. Sie heißt Terminalalphabet, ihre Elemente heißen Terminalsymbole von G.

- P ⊆ (V ∪ Σ)+ × (V ∪ Σ)∗ und P ist endlich. Die Elemente von P heißen Produktionen oder Regeln von G. Eine Produktion (w1, w2) ∈ P wiMD w1 → w2 dargestellt.

- S ∈ V . Dieses ausgezeichnete Nichtterminalsymbol heißt die Startvariable oder das Startsymbol von G.

- Eine Grammatik G = (V, Σ, P, S) heißt kontextfrei, falls für alle Regeln w1 → w2 ∈ P gilt: w1 ∈ V (d. h., die linke Seite der Regel besteht aus genau einem Nichtterminalsymbol).

- Eine Grammatik G = (V, Σ, P, S) heißt rechtslinear, falls für alle Regeln w1 → w2 ∈ P gilt: w1 ∈ V (d. h., G ist kontextfrei) sowie w2 ∈ Σ ∗ oder w2 = uA mit u ∈ Σ ∗ und A ∈ V .

- Eine Grammatik G = (V, Σ, P, S) heißt linkslinear, falls für alle Regeln w1 → w2 ∈ P gilt: w1 ∈ V (d. h. G ist kontextfrei) sowie w2 ∈ Σ ∗ oder w2 = Au mit u ∈ Σ ∗ und A ∈ V .

- Eine Grammatik G = (V, Σ, P, S) heißt regulär, falls sie linkslinear oder rechtslinear ist.


### LL(1) - Grammar

Backus Naur Form von F:
```
Programm             ::= Definition ";" { Definition ";"} .
Definition           ::= Variable {Variable} "=" Ausdruck .
Lokaldefinitionen    ::= Lokaldefinition { ";" Lokaldefinition } .
Lokaldefinition      ::= Variable "=" Ausdruck .
Ausdruck             ::= "let" Lokaldefinitionen "in" Ausdruck
                       | "if" Ausdruck "then" Ausdruck "else" Ausdruck
                       | Ausdruck BinärOp Ausdruck
                       | UnärOp Ausdruck
                       | Ausdruck Ausdruck
                       | "(" Ausdruck ")"
                       | AtomarerAusdruck .
BinärOp              ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
UnärOp               ::= "not" | "−" .
AtomarerAusdruck     ::= Variable | Zahl | Wahrheitswert .
Variable             ::= Name .
```

Backus Naur Form Remove Left Recursion of F:
```
Programm             ::= Definition ";" { Definition ";"} .
First(Programm) = {}

Definition           ::= Variable {Variable} "=" Ausdruck .
Lokaldefinitionen    ::= Lokaldefinition { ";" Lokaldefinition } .
Lokaldefinition      ::= Variable "=" Ausdruck .
Ausdruck             ::= "let" Lokaldefinitionen "in" Ausdruck
                       | "if" Ausdruck "then" Ausdruck "else" Ausdruck
                       | Ausdruck BinärOp Ausdruck
                       | UnärOp Ausdruck
                       | Ausdruck Ausdruck
                       | "(" Ausdruck ")"
                       | AtomarerAusdruck .
BinärOp              ::= "&" | "|" | "==" | "<" | "+" | "−" | "∗" | "/" .
UnärOp               ::= "not" | "−" .
AtomarerAusdruck     ::= Variable | Zahl | Wahrheitswert .
Variable             ::= Name .
```

Abstract Form:
```
P   ->  D ";" | D ";" P
D   ->  V D | V "=" A
LDN ->  LD | LD ";" LDN
LD  ->  V "=" A
A   ->  "let" LDN "in" A 
    |   "if" A "then" A "else" A 
    |   A B A 
    |   U A 
    |   A A 
    |   "(" A ")"
    |   AA
B   ->  "&" 
    |   "|" 
    |   "=="
    |   "<"
    |   "+"
    |   "-"
    |   "*"
    |   "/"
U   ->  "not" |   "-"
AA  ->  V | Z | W
V   ->  N
```

P solved
```
P -> D; P -> D; D; P -> D; D; D;
since \eps is nothing, these are equal
P -> D; MD -> D; P -> D; D; MD -> D; D; P -> D; D; D; MD -> D; D; D; \eps
```

D solved
```
D -> V D -> V V D -> V V V D -> V V V V "=" A
since \eps is nothing, these are equal
D -> V MV "=" A -> V V MV "=" A -> A -> V V V MV "=" A -> V V V V MV "=" A -> V V V V \eps "=" A
```

A solved
```
A -> A B A -> "let" LDN "in" A B A -> "let" LDN "in" U A B A ->
"let" LDN "in" U A B A A -> "let" LDN "in" U A / A A -> 
since \eps is nothing, these are equal
A -> "let" LDN "in" A RA -> "let" LDN "in" A B A RA -> "let" LDN "in" U A RA B A RA -> "let" LDN "in" U A \eps B A RA -> "let" LDN "in" U A \eps B A A RA -> "let" LDN "in" U A \eps B A A \eps -> "let" LDN "in" U A \eps / A A \eps
```

Without Left Recursion:
```
P   ->  D ";" MD
MD  ->  \eps | P

D   ->  V MV "=" A
MV  -> \eps | V MV

LDN ->  LD MLD
MLD -> \eps | ";" LD MLD

LD  ->  V "=" A

A   ->  "let" LDN "in" A RA
    |   "if" A "then" A "else" A RA 
    |   U A RA
    |   "(" A ")" RA
RA  -> \eps | B A RA | A RA 

B   ->  "&" 
    |   "|" 
    |   "=="
    |   "<"
    |   "+"
    |   "-"
    |   "*"
    |   "/"
U   ->  "not" |   "-"
AA  ->  V | Z | W
V   ->  N
```

Without Left Recursion:
```
Programm                ->  Definition ";" RestProgramm
RestProgramm            ->  \eps | Programm

Definition              ->  Variable MultiVariablen "=" Ausdruck
MultiVariablen          ->  \eps | Variable MultiVariablen

Lokaldefinitionen       ->  Lokaldefinition RestLokaldefinitionen
RestLokaldefinitionen   ->  \eps | ";" Lokaldefinition RestLokaldefinitionen

Lokaldefinition         ->  Variable "=" Ausdruck

Ausdruck                ->  "let" Lokaldefinitionen "in" Ausdruck RestAusdruck
                        |   "if" Ausdruck "then" Ausdruck "else" Ausdruck RestAusdruck 
                        |   UnärOp Ausdruck RestAusdruck
                        |   "(" Ausdruck ")" RestAusdruck
                        |   AtomarerAusdruck RestAusdruck

RestAusdruck            ->  \eps 
                        |   BinärOp Ausdruck RestAusdruck 
                        |   Ausdruck RestAusdruck

BinärOp                 ->  "&" 
                        |   "|" 
                        |   "=="
                        |   "<"
                        |   "+"
                        |   "-"
                        |   "*"
                        |   "/"

UnärOp                  ->  "not" 
                        |   "-"

AtomarerAusdruck        ->  Variable 
                        |   Zahl 
                        |   Wahrheitswert

Variable                ->  Name
```

Von Phillip:
```
program             ::= definition ; {definition ;}
definition          ::= Variable {Variable} = expression
localDefinitions    ::= localDefinition {; localDefinition}
localDefinition     ::= Variable = expression
expression          ::=
                        Let localDefinitions In audruck |
                        If expression Then expression Else expression |
                        expression1

expression1         ::= expression2 restExpression1
restExpression1     ::= "|" expression1 | \epsilon
expression2         ::= expression3 restExpression2
restExpression2     ::= & expression2 | \epsilon
expression3         ::= expression4
expression4         ::= expression5 restExpression4
restExpression4     ::= comparisonOperator expression5 | \epsilon
expression5         ::= expression6
expression6         ::= expression7 restExpression6
restExpression6     ::= - expression7 | {+ expression7}
expression7         ::= expression8 restExpression7
restExpression7     ::= / expression8 | {* expression7}
expression8         ::= atomicExpression {atomicExpression}

atomicExpression    ::= Variable | Literal | ( expression )

comparisonOperator  ::= == | <
```

Von Phillip Angepasst:
```
Program                 ::= Definition ; RestProgramm
RestProgramm            ::= \eps | Definition ;

Definition              ::= Var RestVars = Expression
RestVars                ::= \eps | Var RestVars

LocalDefinitions        ::= LocalDefinition RestLocalDefinitions
RestLocalDefinitions    ::= \eps | ; LocalDefinitions

LocalDefinition         ::= Var = Expression

Expression              ::= let LocalDefinitions in Expression |
                            if Expression then Expression else Expression |
                            Expression1

Expression1             ::= Expression2 RestExpression1
RestExpression1         ::= \eps | "|" Expression1

Expression2             ::= Expression3 RestExpression2
RestExpression2         ::= \eps | & Expression2

Expression3             ::= Expression4 RestExpression3
RestExpression3         ::= \eps | CompOp Expression3

Expression4             ::= Expression5 RestExpression4
RestExpression4         ::= \eps | + Expression5 | - Expression5

Expression5             ::= Expression6 | - Expression6

-- Alternative to Expression5
-- Expression5             ::= RestExpression5 Expression6
-- RestExpression5         ::= \eps | -


Expression6             ::= Expression7 RestExpression6
RestExpression6         ::= \eps | * Expression7 | / Expression7

Expression7             ::= AtomicExpression RestExpression7
RestExpression7         ::= \eps | Expression7

AtomicExpression        ::= Var | Int | Bool | ( Expression )

CompOp                  ::= == | <
```