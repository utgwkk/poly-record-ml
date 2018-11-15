# poly-record-ml

## requirements

- menhir
- ounit (for testing)

## BNF

```
<program> ::= <term> ";;"

<term> ::= <variable>
         | <integer>
         | <boolean>
         | <term> <op> <term>
         | "if" <term> "then" <term> "else" <term>
         | "fun" <variable>+ "->" <term>
         | <term> <term>
         | "let" <variable>+ "=" <term> "in" <term>
         | "{" <record> "}"
         | <term> "." <label>
         | "modify" "(" <term> "," <label> "," <term> ")"
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

## References

- Ohori, A. A polymorphic record calculus and its compilation. TOPLAS, 1995. https://dl.acm.org/citation.cfm?id=218572
