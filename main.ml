let debug = ref false

let if_debug f = if !debug then f ()

let rec repl () =
  print_string "# ";
  flush stdout;

  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  if_debug (fun () -> Printf.printf "(* Input *)\n%s\n\n" (ExplicitlyTyped.string_of_exp exp));

  begin try
    let pty = Typechecker.start exp in
    if_debug (fun () -> Printf.printf "(* Type check *)\n%s\n\n" (ExplicitlyTyped.string_of_polyty pty));
  with
    | Typechecker.Typecheck_failed -> retry "[ERROR] type check failed"
    | Typechecker.Kindcheck_failed -> retry "[ERROR] kind check failed"
  end;

  let compiled = Compiler.start exp in
  if_debug (fun () -> Printf.printf "(* Compiled exp *)\n%s\n\n" (Implementation.string_of_exp compiled));
  let value = Evaluator.start compiled in
  Printf.printf "val - = %s\n" (Implementation.string_of_value value);
  repl ()

and retry s =
  print_endline s;
  repl ()

let main () =
  let usage = Printf.sprintf "%s [--debug]" Sys.argv.(0) in
  let arg = Arg.align [
    ("--debug", Arg.Set debug, "print debug output.");
  ] in
  Arg.parse arg (fun _ -> ()) usage;
  repl ()

let () = main ()
