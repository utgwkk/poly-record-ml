open Syntax

type value =
  | VInt of int
  | VRecord of (id * value) list

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VRecord xs -> "{" ^ String.concat ", " (List.map string_of_record_entry xs) ^ "}"
and string_of_record_entry (k, v) = Printf.sprintf "%s=%s" k (string_of_value v)

let rec eval env = function
  | Var x -> Env.lookup x env
  | Int i -> VInt i
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = Env.extend x v1 env in
      eval env' e2
  | Record xs ->
      let xs' = List.sort compare xs in
      VRecord (List.map (fun (k, e) -> (k, eval env e)) xs')
