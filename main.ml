let rec repl () =
  print_string "# ";
  flush stdout;
  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  ignore @@ Typechecker.start exp;
  let compiled = Compiler.start exp in
  let value = Evaluator.start compiled in
  Printf.printf "val - = %s\n" (Implementation.string_of_value value);
  repl ()

let main () =
  repl ()

let () = main ()
