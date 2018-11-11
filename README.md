# poly-record-ml
## BNF

```
<monotype> ::= <typevar>
             | <basetype>
             | <monotype> "->" <monotype>
             | "{" <recordtype_body> "}"
<typevar> ::= "'t" <number>
<basetype> ::= "int" | "bool"
<recordtype_body> ::= <recordtype_field> ("," <recordtype_field>)*
<recordtype_field> ::= <label> ":" <monotype>

<polytype> ::= "forall" <typevar_bounds> "." <monotype>
             | <monotype>

<typevar_bounds> ::= <typevar_bound> ("," <typevar_bound>)*
<typevar_bound> ::= <typevar>
                  | <typevar> "::" <kind>

<kind> ::= <record_kind>
<record_kind> ::= "#{" <recordtype_body> "}"

<term> ::= <variable> <monotype>*
         | <integer>
         | <boolean>
         | <term> <op> <term>
         | "if" <term> "then" <term> "else" <term>
         | "fun" "(" <variable> ":" <monotype> ")" "->" <term>
         | <term> <term>
         | "poly" "(" <term> ":" <polytype> ")"
         | "let" <variable> ":" <polytype> "=" <term> "in" <term>
         | "{" <record> "}"
         | <term> ":" <term> "." <label>
         | "modify" "(" <term> "," <monotype> "," <label> "," <term> ")"
<variable> ::= <alphabet> <alphanum>+
<alphanum> ::= <alphabet> | <number>
<integer> ::= '-'? <number>
<number> ::= <digit>+
<alphabet> ::= ['a' - 'z']
<digit> ::= ['0' - '9']
<boolean> ::= "true" | "false"
<record_body> ::= <record_field> ("," <record_field>)*
<field> ::= <label> "=" <term>
<label> ::= <variable>
```
