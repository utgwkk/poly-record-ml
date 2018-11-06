open Syntax

type idx = Lambda_let_paren.idx

type ty =
  | TVar of tyvar
  | TInt
  | TFun of ty * ty
  | TRecord of (label * ty) list

and kind =
  | KUniv
  | KRecord of (label * ty) list

type polyty = Forall of (tyvar * kind) list * ty

type exp =
  | EPolyInst of id * ty list
  | EInt of int
  | EAbs of id * ty * exp
  | EApp of exp * exp
  | EPolyGen of exp * polyty
  | ELet of id * polyty * exp * exp
  | ERecord of (label * exp) list
  | ERecordGet of exp * ty * label
  | ERecordModify of exp * ty * label * exp

type tyenv = (id, polyty) Environment.t

type lbenv = (label, idx) Environment.t
