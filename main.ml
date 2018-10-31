let rec repl () =
  print_string "# ";
  flush stdout;
  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  let value = Eval.eval Env.empty exp in
  Printf.printf ": %s\n" (Eval.string_of_value value);
  repl ()

let () = repl ()
