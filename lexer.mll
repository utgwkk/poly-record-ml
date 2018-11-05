{
  open Parser
  let reservedWords = [
    ("let", LET);
    ("in", IN);
    ("fun", FUN);
    ("true", TRUE);
    ("false", FALSE);
    ("modify", MODIFY);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
  ]
  |> List.sort compare
}

rule main = parse
| [' ' '\t' '\r' '\n']+ { main lexbuf }
| ";;" { SEMISEMI }
| '-'? ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ['a'-'z'] ['a'-'z']* {
    let id = Lexing.lexeme lexbuf in
    try List.assoc id reservedWords
    with Not_found -> ID id
  }
| '+' { PLUS }
| '*' { MULT }
| '<' { LT }
| '=' { EQ }
| ',' { COMMA }
| '.' { DOT }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LMPAREN }
| '}' { RMPAREN }
| "->" { RARROW }
| eof { exit 1 }
