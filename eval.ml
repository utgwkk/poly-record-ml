open Syntax

exception Runtime_error of string
exception Field_not_found of id
exception Duplicate_field of id

let runtime_error s = raise (Runtime_error s)

type value =
  | VInt of int
  | VRecord of (id * value) list

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VRecord xs -> "{" ^ String.concat ", " (List.map string_of_record_entry xs) ^ "}"
and string_of_record_entry (k, v) = Printf.sprintf "%s=%s" k (string_of_value v)

let rec check_duplicate_field fset = function
	| [] -> ()
	| h :: t ->
      if MySet.member h fset then raise (Duplicate_field h)
      else check_duplicate_field (MySet.insert h fset) t

let rec eval env = function
  | Var x -> Env.lookup x env
  | Int i -> VInt i
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = Env.extend x v1 env in
      eval env' e2
  | Record xs ->
      let xs' = List.sort compare xs in
      check_duplicate_field MySet.empty (List.map fst xs');
      VRecord (List.map (fun (k, e) -> (k, eval env e)) xs')
  | RecordGet (f, e) ->
      let v = eval env e in
      begin match v with
        | VRecord xs -> begin
          try List.assoc f xs
          with Not_found -> raise (Field_not_found f)
        end
        | _ -> runtime_error "Not a record type"
      end
  | RecordModify (e1, f, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      begin match v1 with
        | VRecord xs -> VRecord (List.map (fun (l, v) -> if f = l then (l, v2) else (l, v)) xs)
        | _ -> runtime_error "Not a record type"
      end
