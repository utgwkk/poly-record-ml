let debug = ref false
let srcfile = ref "-"

let if_debug f = if !debug then f ()

let rec compile prompt chan k =
  print_string prompt;
  flush stdout;

  try
    let exp = Parser.main Lexer.main (Lexing.from_channel chan) in
    if_debug (fun () -> Printf.printf "(* Input *)\n%s\n\n" (PolyRecord.string_of_exp exp));

    let (exp, kenv, pty) = Infer.start exp in
    if_debug (fun () ->
      Printf.printf "(* Type inference *)\n%s\n%s\n\n" (ExplicitlyTyped.string_of_exp exp) (PolyRecord.pp_polyty pty);
    );

    let pty = Typechecker.start kenv exp in
    if_debug (fun () -> Printf.printf "(* Type check *)\n%s\n\n" (PolyRecord.string_of_polyty pty));

    let compiled = Compiler.start kenv exp in
    if_debug (fun () -> Printf.printf "(* Compiled exp *)\n%s\n\n" (Implementation.string_of_exp compiled));
    let value = Evaluator.start compiled in
    Printf.printf "val - = %s\n" (Implementation.string_of_value value);
    k ()
  with
    | Infer.Not_bound x -> retry k ("[ERROR] unbound value " ^ x)
    | Infer.Unification_failed s -> retry k ("[ERROR] type inference failed: " ^ s) 
    (* | Typechecker.Typecheck_failed -> retry k "[ERROR] type check failed" *)
    | Typechecker.Kindcheck_failed -> retry k "[ERROR] kind check failed"
    | Evaluator.RuntimeError s -> retry k ("[ERROR] runtime error" ^ s)

and retry k s =
  print_endline s;
  k ()

let main () =
  let usage = Printf.sprintf "%s [--debug] [filename]" Sys.argv.(0) in
  let arg = Arg.align [
    ("--debug", Arg.Set debug, "print debug output.");
  ] in
  Arg.parse arg (fun s -> srcfile := s) usage;

  if !srcfile = "-" then
    let chan = stdin in
    let rec k () = compile "# " chan k in
    compile "# " chan k
  else
    let chan = open_in !srcfile in
    let rec k () = close_in chan in
    compile "" chan k

let () = main ()
