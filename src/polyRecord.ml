open Syntax

type ty =
  | TVar of tyvar
  | TInt
  | TBool
  | TUnit
  | TFun of ty * ty
  | TRecord of (label * ty) list

and kind =
  | KUniv
  | KRecord of (label * ty) list

type polyty = Forall of (tyvar * kind) list * ty

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
  | ERecord of (label * exp) list
  | ERecordGet of exp * label
  | ERecordModify of exp * label * exp
  | EStatement of exp * exp

let rec string_of_ty = function
  | TVar i -> "TVar " ^ string_of_int i
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TUnit -> "TUnit"
  | TFun (t1, t2) -> Printf.sprintf "TFun (%s, %s)" (string_of_ty t1) (string_of_ty t2)
  | TRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> Printf.sprintf "\"%s\", %s" l (string_of_ty t))
      in
      Printf.sprintf "TRecord [%s]" (String.concat "; " xs')

let rec string_of_kind = function
  | KUniv -> "KUniv"
  | KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> Printf.sprintf "\"%s\", %s" l (string_of_ty t))
      in
      Printf.sprintf "KRecord [%s]" (String.concat "; " xs')

let string_of_polyty = function
  | Forall (xs, t) ->
      let xs' =
        xs
        |> List.map (fun (i, k) -> Printf.sprintf "%d, %s" i (string_of_kind k))
      in
      Printf.sprintf "Forall ([%s], %s)" (String.concat "; " xs') (string_of_ty t)

let pp_polyty (Forall (bs, t)) =
  let collect_tyvar_polyty (Forall (xs, t)) =
    xs |> List.map fst
  in
  let rec tyvar_map count = function
    | [] -> []
    | h :: t -> (h, "t" ^ string_of_int count) :: tyvar_map (count + 1) t
  in
  let tyvars = tyvar_map 0 (collect_tyvar_polyty (Forall (bs, t)))
  in
  let rec pp_ty t =
    let rec pp_ty' t =
      match t with
      | TInt -> "int"
      | TBool -> "bool"
      | TUnit -> "unit"
      | TVar i -> "'" ^ List.assoc i tyvars
      | TFun (a, b) -> begin match (a, b) with
          | TFun _, _ -> Printf.sprintf "(%s) -> %s" (pp_ty' a) (pp_ty' b)
          | _ -> Printf.sprintf "%s -> %s" (pp_ty' a) (pp_ty' b)
        end
      | TRecord xs ->
          let xs' =
            xs
            |> List.map (fun (l, t) -> Printf.sprintf "%s : %s" l (pp_ty t))
          in
          Printf.sprintf "{%s}" (String.concat ", " xs')
    in pp_ty' t
  and pp_kind = function
    | KUniv -> ""
    | KRecord xs -> "::#" ^ pp_ty (TRecord xs)
  in
  match bs with
  | [] -> pp_ty t
  | _ ->
      let bs' =
        bs
        |> List.map (fun (v, k) -> Printf.sprintf "%s%s" (pp_ty (TVar v)) (pp_kind k))
      in
      Printf.sprintf "forall %s. %s" (String.concat ", " bs') (pp_ty t)

let rec string_of_exp = function
  | EVar x ->
      Printf.sprintf "EVar \"%s\"" x
  | EInt i -> "EInt " ^ string_of_int i
  | EBool b -> "EBool " ^ string_of_bool b
  | EUnit -> "EUnit"
  | EBinOp (op, e1, e2) ->
      let opstr = match op with
      | Plus -> "Plus"
      | Mult -> "Mult"
      | Lt -> "Lt"
      in Printf.sprintf "EBinOp (%s, %s, %s)" opstr (string_of_exp e1) (string_of_exp e2)
  | EIfThenElse (e1, e2, e3) ->
      Printf.sprintf "EIfThenElse (%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EAbs (x, e) ->
      Printf.sprintf "EAbs (\"%s\", %s)" x (string_of_exp e)
  | EUnitAbs e ->
      Printf.sprintf "EUnitAbs %s" (string_of_exp e)
  | EApp (e1, e2) ->
      Printf.sprintf "EApp (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | ELet (x, e1, e2) ->
      Printf.sprintf "ELet (\"%s\", %s, %s)" x (string_of_exp e1) (string_of_exp e2)
  | ERecord xs ->
      let xs' = List.map (fun (l, e) -> Printf.sprintf "(\"%s\", %s)" l (string_of_exp e)) xs in
      Printf.sprintf "ERecord [%s]" (String.concat "; " xs')
  | ERecordGet (e, l) ->
      Printf.sprintf "ERecordGet (%s, \"%s\")" (string_of_exp e)  l
  | ERecordModify (e1, l, e2) ->
      Printf.sprintf "ERecordModify (%s, \"%s\", %s)" (string_of_exp e1) l (string_of_exp e2)
  | EStatement (e1, e2) ->
      Printf.sprintf "EStatement (%s, %s)" (string_of_exp e1) (string_of_exp e2)

let rec ty_eq t1 t2 = match t1, t2 with
  | TVar i1, TVar i2 -> i1 = i2
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TUnit, TUnit -> true
  | TFun (t1, t1r), TFun (t2, t2r) ->
      ty_eq t1 t2 && ty_eq t1r t2r
  | TRecord xs, TRecord ys -> begin
      try List.fold_left2 (fun b (l1, x) (l2, y) -> b && l1 = l2 && ty_eq x y) true xs ys
      with _ -> false
    end
  | _ -> false

and kind_eq k1 k2 = match k1, k2 with
  | KUniv, KUniv -> true
  | KRecord xs, KRecord ys -> begin
      try List.fold_left2 (fun b (l1, x) (l2, y) -> b && l1 = l2 && ty_eq x y) true xs ys
      with _ -> false
    end
  | _ -> false

and polyty_eq (Forall (xs, t1)) (Forall (ys, t2)) =
  (
    try List.fold_left2 (fun b (v1, x) (v2, y) -> b && v1 = v2 && kind_eq x y) true xs ys
    with _ -> false
  ) && ty_eq t1 t2

