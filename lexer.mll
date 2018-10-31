{
  open Parser
  let reservedWords = [
    ("let", LET);
    ("in", IN);
  ]
  |> List.sort compare
}

rule main = parse
| [' ' '\t']+ { main lexbuf }
| ";;" { SEMISEMI }
| '-'? ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ['a'-'z'] ['a'-'z']* {
    let id = Lexing.lexeme lexbuf in
    try List.assoc id reservedWords
    with Not_found -> ID id
  }
| '=' { EQ }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LMPAREN }
| '}' { RMPAREN }
| eof { exit 1 }
