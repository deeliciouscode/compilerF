# A compiler for the language F 
## In context of the course "Implementierung von Programmiersprachen" at LMU University.

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

Provided by Philip
```
Program ::= Definition ; {Definition ;}
Definition ::= Variable {Variable} = Expression
LocalDefinitions ::= LocalDefinition {; LocalDefinition}
LocalDefinition ::= Variable = Expression
Expression ::=
                let LocalDefinitions in Expression |
                if Expression then Expression else Expression |
                Expression1

Expression1 ::= Expression2 {| Expression2}
Expression2 ::= Expression3 {& Expression3}
Expression3 ::= Expression4 [ComparisonOperator Expression4]
Expression4 ::= Expression5 RestExpression4
RestExpression4 ::= {+ Expression5} | - Expression5
Expression5 ::= [-] Expression6
Expression6 ::= Expression7 RestExpression6
RestExpression6 ::= {* Expression7} | / Expression7
Expression7 ::= AtomicExpression {AtomicExpression}
AtomicExpression ::= Variable | Literal | ( Expression )
ComparisonOperator ::= == | <
```

Provided by Phillip modified:
```
Program                 ::= Definition ; RestProgramm
RestProgramm            ::= \eps | Program ;

Definition              ::= Var RestVars = Expression
RestVars                ::= \eps | Var RestVars

LocalDefinitions        ::= LocalDefinition RestLocalDefinitions
RestLocalDefinitions    ::= \eps | ; LocalDefinitions

LocalDefinition         ::= Var = Expression

Expression              ::= let LocalDefinitions in Expression |
                            if Expression then Expression else Expression |
                            Expression1

Expression1             ::= Expression2 RestExpression1
RestExpression1         ::= \eps | "|" Expression2

Expression2             ::= Expression3 RestExpression2
RestExpression2         ::= \eps | & Expression3

Expression3             ::= Expression4 RestExpression3
RestExpression3         ::= \eps | CompOp Expression4

Expression4             ::= Expression5 RestExpression4
RestExpression4         ::= \eps | + Expression5 | - Expression5

Expression5             ::= Expression6 | - Expression6

Expression6             ::= Expression7 RestExpression6
RestExpression6         ::= \eps | * Expression7 | / Expression7

Expression7             ::= AtomicExpression RestExpression7
RestExpression7         ::= \eps | AtomicExpression

AtomicExpression        ::= Var | Int | Bool | ( Expression )

CompOp                  ::= == | <
```

-----------------------------------------------------------------

## Some evaluations of the current state

-----------------------------------------------------------------

"(1 + 2) * 3;"

Just 
(Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 
(Parenthesised (Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 (AtomExpr (T_INT 1)) RE7eps) RE6eps)) 
(PLUS (PosExpr5 (Expr6 (Expr7 (AtomExpr (T_INT 2)) RE7eps) RE6eps)))) RE3eps) RE2eps) RE1eps))) RE7eps) 
(MULT (Expr7 (AtomExpr (T_INT 3)) RE7eps)))) RE4eps) RE3eps) RE2eps) RE1eps))

------------------------------------------------------------------

"(foo bar) 1 * 3;"

(Just 
(Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 
(Parenthesised (Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 (AtomExpr (T_VAR (Name "foo"))) 
(App (Expr7 (AtomExpr (T_VAR (Name "bar"))) RE7eps))) RE6eps)) RE4eps) RE3eps) RE2eps) RE1eps))) 
(App (Expr7 (AtomExpr (T_INT 1)) RE7eps))) 
(MULT (Expr7 (AtomExpr (T_INT 3)) RE7eps)))) RE4eps) RE3eps) RE2eps) RE1eps)),[])

------------------------------------------------------------------

"foo bar zoom 1;"

Just 
(Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 (AtomExpr (T_VAR (Name "foo"))) 
(App (Expr7 (AtomExpr (T_VAR (Name "bar"))) 
(App (Expr7 (AtomExpr (T_VAR (Name "zoom"))) 
(App (Expr7 (AtomExpr (T_INT 1)) RE7eps))))))) RE6eps)) RE4eps) RE3eps) RE2eps) RE1eps))

------------------------------------------------------------------

"(1 * (2 + (3 + 4))) - (5 / 6);"

Just 
(Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 
(Parenthesised (Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 (AtomExpr 
(T_INT 1)) RE7eps) 
(MULT (Expr7 
(Parenthesised (Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 (AtomExpr 
(T_INT 2)) RE7eps) RE6eps)) 
(PLUS (PosExpr5 (Expr6 (Expr7 
(Parenthesised (Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 (AtomExpr 
(T_INT 3)) RE7eps) RE6eps)) 
(PLUS (PosExpr5 (Expr6 (Expr7 (AtomExpr 
(T_INT 4)) RE7eps) RE6eps)))) RE3eps) RE2eps) RE1eps))) RE7eps) RE6eps)))) RE3eps) RE2eps) RE1eps))) RE7eps)))) RE4eps) RE3eps) RE2eps) RE1eps))) RE7eps) RE6eps)) 
(MINUS (PosExpr5 (Expr6 (Expr7 
(Parenthesised (Expr (Expr1 (Expr2 (Expr3 (Expr4 (PosExpr5 (Expr6 (Expr7 (AtomExpr 
(T_INT 5)) RE7eps) 
(DIV (Expr7 (AtomExpr 
(T_INT 6)) RE7eps)))) RE4eps) RE3eps) RE2eps) RE1eps))) RE7eps) RE6eps)))) RE3eps) RE2eps) RE1eps))

------------------------------------------------------------------