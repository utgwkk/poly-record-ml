open Syntax

type ty = PolyRecord.ty
type polyty = PolyRecord.polyty

type exp =
  | EPolyInst of id * ty list
  | EInt of int
  | EBool of bool
  | EUnit
  | EBinOp of binOp * exp * exp
  | EIfThenElse of exp * exp * exp
  | EAbs of id * ty * exp
  | EUnitAbs of exp
  | EApp of exp * exp
  | EPolyGen of exp * polyty
  | ELet of id * polyty * exp * exp
  | ERecord of exp record
  | ERecordGet of exp * ty * label
  | ERecordModify of exp * ty * label * exp
  | ERecordAssign of exp * ty * label * exp
  | EStatement of exp * exp
  | ERef of exp
  | EDeref of exp

let rec erase = function
  | EPolyInst (id, _) -> PolyRecord.EVar id
  | EInt i -> PolyRecord.EInt i
  | EBool b -> PolyRecord.EBool b
  | EUnit -> PolyRecord.EUnit
  | EBinOp (op, e1, e2) -> PolyRecord.EBinOp (op, erase e1, erase e2)
  | EIfThenElse (e1, e2, e3) -> PolyRecord.EIfThenElse (erase e1, erase e2, erase e3)
  | EAbs (id, _, e) -> PolyRecord.EAbs (id, erase e)
  | EUnitAbs e -> PolyRecord.EUnitAbs (erase e)
  | EApp (e1, e2) -> PolyRecord.EApp (erase e1, erase e2)
  | EPolyGen (e, _) -> erase e
  | ELet (id, _, e1, e2) -> PolyRecord.ELet (id, erase e1, erase e2)
  | ERecord xs ->
      let xs' = List.map (fun (l, e) -> (l, erase e)) xs in
      PolyRecord.ERecord xs'
  | ERecordGet (e, _, l) -> PolyRecord.ERecordGet (erase e, l)
  | ERecordModify (e1, _, l, e2) -> PolyRecord.ERecordModify (erase e1, l, erase e2)
  | ERecordAssign (e1, _, l, e2) -> PolyRecord.ERecordAssign (erase e1, l, erase e2)
  | EStatement (e1, e2) -> PolyRecord.EStatement (erase e1, erase e2)
  | ERef e -> PolyRecord.ERef (erase e)
  | EDeref e -> PolyRecord.EDeref (erase e)

let is_value e = PolyRecord.is_value (erase e)

let rec string_of_exp = function
  | EPolyInst (x, xs) ->
      let xs' =
        xs
        |> List.map PolyRecord.string_of_ty
      in
      Printf.sprintf "EPolyInst (\"%s\", [%s])" x (String.concat "; " xs')
  | EInt i -> "EInt " ^ string_of_int i
  | EBool b -> "EBool " ^ string_of_bool b
  | EUnit -> "EUnit"
  | EBinOp (op, e1, e2) ->
      let opstr = string_of_binOp op
      in Printf.sprintf "EBinOp (%s, %s, %s)" opstr (string_of_exp e1) (string_of_exp e2)
  | EIfThenElse (e1, e2, e3) ->
      Printf.sprintf "EIfThenElse (%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EAbs (x, t, e) ->
      Printf.sprintf "EAbs (\"%s\", %s, %s)" x (PolyRecord.string_of_ty t) (string_of_exp e)
  | EUnitAbs e ->
      Printf.sprintf "EUnitAbs (%s)" (string_of_exp e)
  | EApp (e1, e2) ->
      Printf.sprintf "EApp (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | EPolyGen (e, pt) ->
      Printf.sprintf "EPolyGen (%s, %s)" (string_of_exp e) (PolyRecord.string_of_polyty pt)
  | ELet (x, pt, e1, e2) ->
      Printf.sprintf "ELet (\"%s\", %s, %s, %s)" x (PolyRecord.string_of_polyty pt) (string_of_exp e1) (string_of_exp e2)
  | ERecord xs ->
      let xs' = List.map (fun (l, e) -> Printf.sprintf "(\"%s\", %s)" l (string_of_exp e)) xs in
      Printf.sprintf "ERecord [%s]" (String.concat "; " xs')
  | ERecordGet (e, t, l) ->
      Printf.sprintf "ERecordGet (%s, %s, \"%s\")" (string_of_exp e) (PolyRecord.string_of_ty t) l
  | ERecordModify (e1, t, l, e2) ->
      Printf.sprintf "ERecordModify (%s, %s, \"%s\", %s)" (string_of_exp e1) (PolyRecord.string_of_ty t) l (string_of_exp e2)
  | ERecordAssign (e1, t, l, e2) ->
      Printf.sprintf "ERecordAssign (%s, %s, \"%s\", %s)" (string_of_exp e1) (PolyRecord.string_of_ty t) l (string_of_exp e2)
  | EStatement (e1, e2) ->
      Printf.sprintf "EStatement (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | ERef e ->
      Printf.sprintf "ERef (%s)" (string_of_exp e)
  | EDeref e ->
      Printf.sprintf "EDeref (%s)" (string_of_exp e)
