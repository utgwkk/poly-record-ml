open Syntax

type idxvar = int

type idx =
  | IVar of idxvar
  | INat of int

type ty =
  | TVar of tyvar
  | TInt
  | TBool
  | TUnit
  | TFun of ty * ty
  | TRecord of (label * ty) list
  | TRef of ty
  | TIdxFun of idxty list * ty
and idxty = label * ty
and kind =
  | KUniv
  | KRecord of (label * ty) list

type polyty = Forall of (tyvar * kind) list * ty

type subst = (tyvar * ty) list

let substitute subs t =
  let rec inner (tv, t) = function
    | TVar tv' -> if tv = tv' then t else TVar tv'
    | TFun (t1, t2) -> TFun (inner (tv, t) t1, inner (tv, t) t2)
    | TRecord xs ->
        let xs' = List.map (fun (l, t') -> (l, inner (tv, t') t)) xs in
        TRecord xs'
    | TIdxFun (xs, t') ->
        TIdxFun (xs, inner (tv, t) t')
    | TRef t -> TRef (inner (tv, t) t)
    | t -> t
  in List.fold_right inner subs t

exception Label_not_found of label
exception Undefined_index_value
exception Not_a_record_type

let idx_value l =
  let rec inner n = function
    | [] -> raise (Label_not_found l)
    | (l', _) :: t ->
        if l = l' then n
        else inner (n + 1) t
  in function
  | TVar _ -> raise Undefined_index_value
  | TRecord xs -> inner 1 xs
  | _ -> raise Not_a_record_type

type exp =
  | EVar of id
  | EInt of int
  | EBool of bool
  | EUnit
  | EBinOp of binOp * exp * exp
  | EIfThenElse of exp * exp * exp
  | EAbs of id * exp
  | EUnitAbs of exp
  | EApp of exp * exp
  | ELet of id * exp * exp
  | EArray of exp list
  | EArrayGet of exp * idx
  | EArrayModify of exp * idx * exp
  | EArrayAssign of exp * idx * exp
  | EIdxAbs of idxvar * exp
  | EIdxApp of exp * idx
  | EStatement of exp * exp
  | ERef of exp
  | EDeref of exp

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VProc of id * exp * env * idxenv
  | VArray of value array
  | VIdxAbs of idxvar * exp * env
  | VRef of value ref

and env = (id, value) Environment.t

and idxenv = (idxvar, idx) Environment.t

type lbenv = (idxty, idx) Environment.t

let string_of_idx = function
  | IVar i -> "IVar " ^ string_of_int i
  | INat i -> "INat " ^ string_of_int i

let rec string_of_exp = function
  | EVar x -> "EVar \"" ^ x ^ "\""
  | EInt i -> "EInt " ^ string_of_int i
  | EBool b -> "EBool " ^ string_of_bool b
  | EUnit -> "EUnit"
  | EBinOp (op, e1, e2) ->
      let opstr = string_of_binOp op
      in Printf.sprintf "EBinOp (%s, %s, %s)" opstr (string_of_exp e1) (string_of_exp e2)
  | EIfThenElse (e1, e2, e3) ->
      Printf.sprintf "EIfThenElse (%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EAbs (x, e) ->
      Printf.sprintf "EAbs (\"%s\", %s)" x (string_of_exp e)
  | EUnitAbs e ->
      Printf.sprintf "EUnitAbs (%s)" (string_of_exp e)
  | EApp (e1, e2) ->
      Printf.sprintf "EApp (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | ELet (x, e1, e2) ->
      Printf.sprintf "ELet (\"%s\", %s, %s)" x (string_of_exp e1) (string_of_exp e2)
  | EArray xs ->
      let xs' = List.map string_of_exp xs in
      Printf.sprintf "EArray [%s]" (String.concat "; " xs')
  | EArrayGet (e, i) ->
      Printf.sprintf "EArrayGet (%s, %s)" (string_of_exp e) (string_of_idx i)
  | EArrayModify (e1, i, e2) ->
      Printf.sprintf "EArrayModify (%s, %s, %s)" (string_of_exp e1) (string_of_idx i) (string_of_exp e2)
  | EArrayAssign (e1, i, e2) ->
      Printf.sprintf "EArrayAssign (%s, %s, %s)" (string_of_exp e1) (string_of_idx i) (string_of_exp e2)
  | EIdxAbs (i, e) ->
      Printf.sprintf "EIdxAbs (%d, %s)" i (string_of_exp e)
  | EIdxApp (e, i) ->
      Printf.sprintf "EIdxApp (%s, %s)" (string_of_exp e) (string_of_idx i)
  | EStatement (e1, e2) ->
      Printf.sprintf "EStatement (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | ERef e ->
      Printf.sprintf "ERef (%s)" (string_of_exp e)
  | EDeref e ->
      Printf.sprintf "EDeref (%s)" (string_of_exp e)

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VProc _ -> "<fun>"
  | VArray arr ->
      let xs = Array.to_list arr in
      "{" ^ String.concat ", " (List.map string_of_value xs) ^ "}"
  | VIdxAbs _ -> "<ifun>"
  | VRef _ -> "<ref>"
