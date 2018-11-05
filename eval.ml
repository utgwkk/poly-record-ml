open Syntax

exception Runtime_error of string
exception Field_not_found of id
exception Duplicate_field of id

let runtime_error s = raise (Runtime_error s)

type value =
  | VInt of int
  | VBool of bool
  | VFun of id * exp * value Env.t
  | VRecord of (id * value) list

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VFun _ -> "<fun>"
  | VRecord xs -> "{" ^ String.concat ", " (List.map string_of_record_entry xs) ^ "}"
and string_of_record_entry (k, v) = Printf.sprintf "%s=%s" k (string_of_value v)

let eval_binop op v1 v2 = match (op, v1, v2) with
  | (Plus, VInt lhs, VInt rhs) -> VInt (lhs + rhs)
  | (Mult, VInt lhs, VInt rhs) -> VInt (lhs * rhs)
  | (Lt, VInt lhs, VInt rhs) -> VBool (lhs < rhs)
  | (_, VInt lhs, invalid_rhs) -> runtime_error (string_of_value invalid_rhs ^ " is not an integer")
  | (_, invalid_lhs, _) -> runtime_error (string_of_value invalid_lhs ^ " is not an integer")

let rec check_duplicate_field fset = function
	| [] -> ()
	| h :: t ->
      if MySet.member h fset then raise (Duplicate_field h)
      else check_duplicate_field (MySet.insert h fset) t

let rec eval env = function
  | Var x -> Env.lookup x env
  | Int i -> VInt i
  | Bool b -> VBool b
  | BinOp (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      eval_binop op v1 v2
  | IfThenElse (e1, e2, e3) ->
      let vc = eval env e1 in
      begin match vc with
      | VBool b -> if b then eval env e2 else eval env e3
      | _ -> runtime_error (string_of_value vc ^ " is not a boolean value")
      end
  | Fun (x, e) ->
      VFun (x, e, env)
  | App (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      begin match v1 with
        | VFun (x, e, env') ->
            let env'' = Env.extend x v2 env' in
            eval env'' e
        | v -> runtime_error (string_of_value v ^ " is not a function")
      end
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
