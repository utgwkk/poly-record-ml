open Syntax

type idxvar = int

type idx =
  | IVar of idxvar
  | INat of int

type ty =
  | TVar of tyvar
  | TInt
  | TFun of ty * ty
  | TRecord of (label * ty) list
  | TIdxFun of (label * ty) list * ty
and kind =
  | KUniv
  | KRecord of (label * ty) list

type polyty = Forall of (tyvar * kind) list * ty

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
  | EAbs of id * exp
  | EApp of exp * exp
  | ELet of id * exp * exp
  | EArray of exp list
  | EArrayGet of exp * idx
  | EArrayModify of exp * idx * exp
  | EIdxAbs of idxvar * exp
  | EIdxApp of exp * idx

type value =
  | VInt of int
  | VProc of id * exp * env
  | VArray of value array
  | VIdxAbs of idxvar * exp

and env = (id, value) Environment.t

and idxenv = (idxvar, idx) Environment.t
