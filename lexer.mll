{
  open Parser

  let reservedWords = [
    ("bool", BOOL);
    ("else", ELSE);
    ("false", FALSE);
    ("forall", FORALL);
    ("fun", FUN);
    ("if", IF);
    ("in", IN);
    ("int", INT);
    ("let", LET);
    ("modify", MODIFY);
    ("poly", POLY);
    ("then", THEN);
    ("true", TRUE);
  ] |> List.sort compare
}

rule main = parse
  | [' ' '\009' '\012' '\n']+     { main lexbuf }
  | "-"? ['0'-'9'] { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";;" { SEMISEMI }
  | "{" { LRECORDPAREN }
  | "}" { RRECORDPAREN }
  | "{{" { KLPAREN }
  | "}}" { KRPAREN }
  | "->" { RARROW }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | "." { DOT }
  | "," { COMMA }
  | "=" { EQ }
  | "+" { PLUS }
  | "*" { MULT }
  | "<" { LT }
  | ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    {
      let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
    }
  | "'" ['0'-'9']+
    {
      let buf = Lexing.lexeme lexbuf in
      let len = String.length buf in
      TVAR (int_of_string (String.sub buf 1 (len - 1))) }
	| eof { exit 0 }
