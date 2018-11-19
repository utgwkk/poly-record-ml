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
         | "()"
         | <term> <op> <term>
         | "if" <term> "then" <term> "else" <term>
         | "fun" (<variable>+ | "()") "->" <term>
         | <term> <term>
         | "let" <variable> (<variable>+ | "()")? "=" <term> "in" <term>
         | "{" <record> "}"
         | <term> "." <label>
         | "modify" "(" <term> "," <label> "," <term> ")"
         | <term> ";" <term>
         | "ref" <term>
         | "!" <term>
<variable> ::= <small> | <alphanum>+
<alphanum> ::= <alphabet> | <number> | ['_' '\'']
<integer> ::= '-'? <number>
<number> ::= <digit>+
<alphabet> ::= <small> | <capital>
<small> ::= ['a' - 'z']
<capital> ::= ['A' - 'Z']
<digit> ::= ['0' - '9']
<boolean> ::= "true" | "false"
<op> ::= "+" | "*" | "<" | ":="
<record_body> ::= <record_field> ("," <record_field>)*
<field> ::= <label> "=" <term>
<label> ::= <variable>
```

## References

- Ohori, A. A polymorphic record calculus and its compilation. TOPLAS, 1995. https://dl.acm.org/citation.cfm?id=218572
- Wright, A. K. Simple imperative polymorphism. Lisp and Symbolic Computation, 1995. https://pdfs.semanticscholar.org/fd1d/87c648622ebbc7f086b26c23a947889bb90d.pdf
