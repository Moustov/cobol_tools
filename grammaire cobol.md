## CALL
    <call-statement> ::= "CALL" <call-target> [ "USING" <using-phrase> ] [ "RETURNING" <returning-phrase> ] [ <exception-phrases> ] "."
    <call-target> ::= <identifier> | <literal> | <procedure-pointer> | <function-pointer>
    <using-phrase> ::= <parameter> { <parameter> }*
    <parameter> ::= [ "BY" ( "REFERENCE" | "CONTENT" | "VALUE" ) ] ( <identifier> | "ADDRESS OF" <identifier> | "LENGTH OF" <identifier> | <literal> | "OMITTED" )
    <returning-phrase> ::= <identifier>
    <exception-phrases> ::= [ "ON EXCEPTION" <imperative-statement> ] [ "NOT ON EXCEPTION" <imperative-statement> ] [ "ON OVERFLOW" <imperative-statement> ] [ "NOT ON OVERFLOW" <imperative-statement> ]
    <identifier> ::= /* Identificateur COBOL valide */
    <literal> ::= /* Littéral COBOL valide */
    <procedure-pointer> ::= /* Pointeur de procédure COBOL valide */
    <function-pointer> ::= /* Pointeur de fonction COBOL valide */
    <imperative-statement> ::= /* Instruction impérative COBOL valide */
    Exemple
    # CALL 'subprogram' USING BY REFERENCE var1 BY VALUE 10 RETURNING result
    # ON EXCEPTION DISPLAY 'Error'
    # NOT ON EXCEPTION DISPLAY 'Success'.


## IF
### BNF1
    /!\ "END-IF"["."]
    c'est en fait "END-IF" ou "." ou "END-IF."
    < if -statement >: :=
    "IF" < condition > < statements > ["ELSE" < statements >]
    "END-IF"["."]
    < condition >: := < simple - condition > | < complex - condition >
    < simple - condition >: := < identifier > ["NOT"] < relational - operator > < literal >
    < complex - condition >: := < simple - condition > {"AND" | "OR"} < simple - condition > *
    < statements >: := < statement > +
    < statement >: := < imperative - statement > | < if -statement >
        < imperative - statement >: := < label > | < cobol
        cmd > | < identifier > ["."]
    < relational - operator >: := r'(=|<>|<|>|<=|>=|IS\s+EQUAL\s+TO|IS\s+NOT\s+EQUAL\s+TO|IS\s+GREATER\s+THAN|IS\s+NOT\s+GREATER\s+THAN|IS\s+LESS\s+THAN|IS\s+NOT\s+LESS\s+THAN|IS\s+GREATER\s+THAN\s+OR\s+EQUAL\s+TO|IS\s+LESS\s+THAN\s+OR\s+EQUAL\s+TO)'
    < identifier >: := < letter > { < letter > | < digit > | "-"} *
    < literal >: := r'[\w\d-]+'
    < operation >: := "MOVE" | "ADD" | "SUBTRACT" | "MULTIPLY" | "DIVIDE" | "INITIALIZE"

### BNF2
    <if-statement> ::= "IF" <condition> <statement> ["ELSE" <statement>] "END-IF" | "."
    <condition> ::= <simple-condition> | <compound-condition>
    <simple-condition> ::= [ "NOT" ] <identifier> <relational-operator> <literal>
    <compound-condition> ::= <simple-condition> { "AND" | "OR" <simple-condition> }
    <relational-operator> ::= "=" | ">" | "<" | ">=" | "<=" | "<>"
                           | "IS EQUAL TO" | "IS NOT EQUAL TO"
                           | "IS GREATER THAN" | "IS NOT GREATER THAN"
                           | "IS LESS THAN" | "IS NOT LESS THAN"
                           | "IS GREATER THAN OR EQUAL TO"
                           | "IS LESS THAN OR EQUAL TO"
                           | "NOT >" | "NOT <" | "NOT ="
    <statement> ::= <cobol-statement> | <if-statement>
    <cobol-statement> ::= <any valid COBOL statement>



 ```python
     literal = rf'(\")?[\w\d-]+(\")?'
     relational_operator = r'(=|<>|<|>|<=|>=|IS\s+EQUAL\s+TO|IS\s+NOT\s+EQUAL\s+TO|IS\s+GREATER\s+THAN|IS\s+NOT\s+GREATER\s+THAN|IS\s+LESS\s+THAN|IS\s+NOT\s+LESS\s+THAN|IS\s+GREATER\s+THAN\s+OR\s+EQUAL\s+TO|IS\s+LESS\s+THAN\s+OR\s+EQUAL\s+TO)'
     # Définir les conditions
     simple_condition = rf'({literal}\s+(NOT\s+)?{relational_operator}\s+{literal}\s*)'
    
     complex_condition = rf'{simple_condition}(\s+(AND|OR)\s+{simple_condition})*'
     # Définir les instructions
     imperative_statement = rf'[^\.]*\n'
     endif = rf'(?P<endif>(END-IF|END-IF\.|[^\.]*\.))'
    
     # Définir l'instruction IF complète avec des groupes nommés
     if_statement = rf'^\s*IF\s+(?P<condition>{complex_condition})\s*\n'
     if_statement += rf'(?P<then_statements>(?:{imperative_statement}\s*)+){endif}'
    
     # Compilation de la regex avec les options de drapeau pour ignorer les espaces et les nouvelles lignes
     if_pattern = re.compile(if_statement, re.VERBOSE | re.DOTALL | re.MULTILINE | re.IGNORECASE)

     matches = if_pattern.finditer(if_parts[0] + '.')
```

### PERFORM
#### BNF1
    <perform-statement> ::= <perform-statement> ::= "PERFORM" <procedure-name> [ "THRU" <procedure-name> ] [ "VARYING" <variable> "FROM" <start-value> "BY" <increment-value> "UNTIL" <condition> ]
        <procedure-name> ::= <identifier>
        <condition> ::= <boolean-expression>
        <integer> ::= <numeric-literal>
        <variable> ::= <identifier>
        <start-value> ::= <numeric-literal>
        <increment-value> ::= <numeric-literal>

    <simple-perform> ::= <paragraph-name>
    <perform-thru> ::= <paragraph-name> "THRU" <paragraph-name>
    <perform-times> ::= <paragraph-name> <integer> "TIMES"
    <perform-until> ::= <paragraph-name> "UNTIL" <condition>
    <perform-varying> ::= <paragraph-name> "VARYING" <identifier> "FROM" <initial-value> "BY" <increment-value> "UNTIL" <condition>
    <paragraph-name> ::= <identifier>
    <condition> ::= <boolean-expression>
    <identifier> ::= <letter> { <letter> | <digit> }*
    <initial-value> ::= <integer>
    <increment-value> ::= <integer>
    <boolean-expression> ::= <identifier> <relational-operator> <value>
    <relational-operator> ::= "=" | ">" | "<" | ">=" | "<=" | "<>"
    <value> ::= <integer> | <identifier>
    <integer> ::= <digit> { <digit> }*
    <letter> ::= "A" | "B" | ... | "Z"
    <digit> ::= "0" | "1" | ... | "9"
    <optional-comment> ::= <comment>*
    <comment> ::= "*" <any-text>
                | "//" <any-text>
    <any-text> ::= { <letter> | <digit> | <symbol> }*
    <symbol> ::= " " | "!" | "\"" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "[" | "\\" | "]" | "^" | "_" | "`" | "{" | "|" | "}" | "~"

#### BNF2 avec CR/LF
    <perform-statement> ::= "PERFORM" <newline> <varying-clause> <newline> <statements> <newline> [<end-perform>]
    
    <varying-clause> ::= "VARYING" <newline> <control-variable> <newline> <from-clause> <newline> <by-clause> <newline> <until-clause> <newline> <after-clause>* <newline>
    
    <from-clause> ::= "FROM" <newline> <start-value> <newline>
    <by-clause> ::= "BY" <newline> <increment-value> <newline>
    <until-clause> ::= "UNTIL" <newline> <condition> <newline>
    <after-clause> ::= "AFTER" <newline> <control-variable> <newline> <from-clause> <newline> <by-clause> <newline> <until-clause> <newline>
    
    <control-variable> ::= identifier
    <start-value> ::= integer
    <increment-value> ::= integer
    <condition> ::= expression
    
    <statements> ::= <statement>* <newline>
    <statement> ::= any valid COBOL statement
    
    <end-perform> ::= "." | "END-PERFORM" <newline>

### BNF avec les commentaires

    <perform-statement> ::= "PERFORM" <newline> <varying-clause> <newline> <statements> <newline> [<end-perform>]
    
    <varying-clause> ::= "VARYING" <newline> <control-variable> <newline> <from-clause> <newline> <by-clause> <newline> <until-clause> <newline> <after-clause>* <newline>
    
    <from-clause> ::= "FROM" <newline> <start-value> <newline>
    <by-clause> ::= "BY" <newline> <increment-value> <newline>
    <until-clause> ::= "UNTIL" <newline> <condition> <newline>
    <after-clause> ::= "AFTER" <newline> <control-variable> <newline> <from-clause> <newline> <by-clause> <newline> <until-clause> <newline>
    
    <control-variable> ::= identifier
    <start-value> ::= integer
    <increment-value> ::= integer
    <condition> ::= expression
    
    <statements> ::= (<statement> | <comment>)* <newline>
    <statement> ::= any valid COBOL statement
    <comment> ::= "*" <any-text> <newline> | "//" <any-text> <newline>
    
    <end-perform> ::= "." | "END-PERFORM" <newline>

### Regex
#### Expression régulière pour correspondre à la grammaire BNF de la commande PERFORM en COBOL
```python
    perform_regex = re.compile(r"""
    ^PERFORM\s+                           # Mot-clé PERFORM suivi d'espaces
    (VARYING\s+\w+\s+FROM\s+\d+\s+BY\s+\d+\s+UNTIL\s+[^\.\n]+(\s+AFTER\s+\w+\s+FROM\s+\d+\s+BY\s+\d+\s+UNTIL\s+[^\.\n]+)*)?  # Clause VARYING optionnelle
    (THRU\s+\w+-\w+)?                     # Clause THRU optionnelle
    (\s*[^\.\n]+)*                        # Instructions COBOL optionnelles
    (\.\s*|END-PERFORM\s*)?               # Fin du bloc PERFORM par un point ou END-PERFORM
    """, re.VERBOSE | re.MULTILINE)
```


#### Expression régulière pour correspondre à la grammaire BNF de la commande PERFORM en COBOL, incluant des lignes de commentaires
```python
    perform_regex = re.compile(r"""
    ^PERFORM\s+                           # Mot-clé PERFORM suivi d'espaces
    (VARYING\s+\w+\s+FROM\s+\d+\s+BY\s+\d+\s+UNTIL\s+[^\.\n]+(\s+AFTER\s+\w+\s+FROM\s+\d+\s+BY\s+\d+\s+UNTIL\s+[^\.\n]+)*)?  # Clause VARYING optionnelle
    (THRU\s+\w+-\w+)?                     # Clause THRU optionnelle
    (\s*[^\.\n]+)*                        # Instructions COBOL optionnelles
    (\.\s*|END-PERFORM\s*)?               # Fin du bloc PERFORM par un point ou END-PERFORM
    (\s*\*.*|\s*//.*)*                    # Lignes de commentaires optionnelles
    """, re.VERBOSE | re.MULTILINE)
```

#### BNF Complète pour PERFORM en COBOL
    <perform-statement> ::= "PERFORM" <perform-options> 
    <perform-options> ::= <perform-range> | <perform-times> | <perform-until> | <perform-varying> 
    <perform-range> ::= <procedure-name> [ "THRU" <procedure-name> ]
    <perform-times> ::= <procedure-name> "TIMES" <integer>
    <perform-until> ::= <procedure-name> "UNTIL" <condition>
    <perform-varying> ::= <procedure-name> "VARYING" <variable> "FROM" <initial-value> "BY" <increment-value> "UNTIL" <condition>
    <procedure-name> ::= <identifier>
    <integer> ::= <digit>+
    <condition> ::= <expression> <relational-operator> <expression>
    <variable> ::= <identifier>
    <initial-value> ::= <expression>
    <increment-value> ::= <expression>
    <expression> ::= <term> { <additive-operator> <term> }*
    <term> ::= <factor> { <multiplicative-operator> <factor> }*
    <factor> ::= <identifier> | <literal> | "(" <expression> ")"
    <relational-operator> ::= "=" | ">" | "<" | ">=" | "<=" | "<>"
    <additive-operator> ::= "+" | "-"
    <multiplicative-operator> ::= "*" | "/"
    <identifier> ::= <letter> { <letter> | <digit> }*
    <literal> ::= <numeric-literal> | <string-literal>
    <numeric-literal> ::= <digit>+
    <string-literal> ::= '"' { <character> }* '"'
    <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    <letter> ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
    <character> ::= <letter> | <digit> | <special-character>
    <special-character> ::= " " | "!" | "\"" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "[" | "\\" | "]" | "^" | "_" | "`" | "{" | "|" | "}" | "~"
_Explications_
* Commentaires: Les commentaires sont représentés par * ou // et peuvent être inclus dans les espaces blancs.
* Sauts de lignes et lignes vides: Les espaces blancs (y compris les sauts de lignes et les lignes vides) sont implicitement gérés par l'analyseur syntaxique. 
 
#### BNF Complète pour PERFORM en COBOL avec Espaces et Sauts de Ligne
    <perform-statement> ::= "PERFORM" <whitespace> <perform-options> <whitespace>
    <perform-options> ::= <perform-range> | <perform-times> | <perform-until> | <perform-varying>
    <perform-range> ::= <procedure-name> <whitespace> [ "THRU" <whitespace> <procedure-name> ]
    <perform-times> ::= <procedure-name> <whitespace> "TIMES" <whitespace> <integer>
    <perform-until> ::= <procedure-name> <whitespace> "UNTIL" <whitespace> <condition>
    <perform-varying> ::= <procedure-name> <whitespace> "VARYING" <whitespace> <variable> <whitespace> "FROM" <whitespace> <initial-value> <whitespace> "BY" <whitespace> <increment-value> <whitespace> "UNTIL" <whitespace> <condition>
    <procedure-name> ::= <identifier>
    <integer> ::= <digit>+
    <condition> ::= <expression> <whitespace> <relational-operator> <whitespace> <expression>
    <variable> ::= <identifier>
    <initial-value> ::= <expression>
    <increment-value> ::= <expression>
    <expression> ::= <term> { <whitespace> <additive-operator> <whitespace> <term> }*
    <term> ::= <factor> { <whitespace> <multiplicative-operator> <whitespace> <factor> }*
    <factor> ::= <identifier> | <literal> | "(" <whitespace> <expression> <whitespace> ")"
    <relational-operator> ::= "=" | ">" | "<" | ">=" | "<=" | "<>"
    <additive-operator> ::= "+" | "-"
    <multiplicative-operator> ::= "*" | "/"
    <identifier> ::= <letter> { <letter> | <digit> }*
    <literal> ::= <numeric-literal> | <string-literal>
    <numeric-literal> ::= <digit>+
    <string-literal> ::= '"' { <character> }* '"'
    <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    <letter> ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
    <character> ::= <letter> | <digit> | <special-character>
    <special-character> ::= " " | "!" | "\"" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "[" | "\\" | "]" | "^" | "_" | "`" | "{" | "|" | "}" | "~"
    <whitespace> ::= " " | "\t" | "\n" | "\r" | "\f"

Explications
Espaces et sauts de ligne: La règle <whitespace> inclut les espaces (" "), les tabulations ("\t"), les nouvelles lignes ("\n"), les retours chariot ("\r") et les sauts de page ("\f"). Ces éléments sont insérés dans les endroits appropriés de la BNF pour représenter les espaces et les sauts de ligne.
