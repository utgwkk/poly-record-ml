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

let alphabet = ['a'-'z']
let digit = ['0'-'9']
let number = digit+
let ident = alphabet (alphabet | digit | ['_' '\''])*

rule main = parse
  | [' ' '\009' '\012' '\n']+     { main lexbuf }
  | "-"? number { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";;" { SEMISEMI }
  | "{" { LRECORDPAREN }
  | "}" { RRECORDPAREN }
  | "#{" { KLPAREN }
  | "->" { RARROW }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | "." { DOT }
  | "," { COMMA }
  | "=" { EQ }
  | "+" { PLUS }
  | "*" { MULT }
  | "<" { LT }
  | ident
    {
      let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
    }
  | "'t" (number as strnum)
    {
      TVAR (int_of_string strnum)
    }
	| eof { exit 0 }
