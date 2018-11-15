{
  open Parser

  let reservedWords = [
    ("else", ELSE);
    ("false", FALSE);
    ("fun", FUN);
    ("if", IF);
    ("in", IN);
    ("let", LET);
    ("modify", MODIFY);
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
  | "->" { RARROW }
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
	| eof { exit 0 }
