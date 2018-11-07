open Lambda_let_dot

exception Typecheck_failed
exception Kindcheck_failed

(* K, T |- M : ? *)
let rec type_check kenv tyenv exp = raise Typecheck_failed

(* entrypoint *)
let start exp = type_check Environment.empty Environment.empty exp
