open Syntax
open PolyRecord
module ET = ExplicitlyTyped

(* S *)
type 'a subst = (tyvar * 'a) list

(* [ty/tyvar]ty *)
let rec subst_ty t (tv, ty) = match t with
  | TVar tv' -> if tv = tv' then ty else TVar tv'
  | TFun (t1, t2) ->
      TFun (subst_ty t1 (tv, ty), subst_ty t2 (tv, ty))
  | TRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, subst_ty t (tv, ty)))
      in TRecord xs'
  | TRef t -> TRef (subst_ty t (tv, ty))
  | t -> t

(* [ty/tyvar]S *)
let subst_subs s subst = List.map (fun (tv, t) -> (tv, subst_ty t s)) subst

(* [ty/tyvar]k *)
let subst_kind s = function
  | KUniv -> KUniv
  | KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, subst_ty t s))
      in KRecord xs'

(* [ty/tyvar]K *)
let subst_kenv s kenv = Environment.map (subst_kind s) kenv

(* S(ty) *)
let apply_subst_to_ty subst t = List.fold_left (fun t s -> subst_ty t s) t subst

(* S(T) *)
let apply_subst_to_tyenv s tyenv =
  List.fold_left (fun env s -> Environment.map (fun (Forall (xs, t)) -> Forall (xs, subst_ty t s)) env) tyenv s

(* [ty/tyvar]E *)
let subst_eqs subst eqs =
  List.map (fun (t1, t2) -> (apply_subst_to_ty subst t1, apply_subst_to_ty subst t2)) eqs

(* S(M) *)
let rec apply_subst_to_exp subs = function
  | ET.EPolyInst (x, ts) ->
      ET.EPolyInst (x, List.map (apply_subst_to_ty subs) ts)
  | ET.EBinOp (op, e1, e2) ->
      ET.EBinOp (op, apply_subst_to_exp subs e1, apply_subst_to_exp subs e2)
  | ET.EIfThenElse (e1, e2, e3) ->
      ET.EIfThenElse (
        apply_subst_to_exp subs e1,
        apply_subst_to_exp subs e2,
        apply_subst_to_exp subs e3
      )
  | ET.EAbs (x, t, e) ->
      ET.EAbs (x, apply_subst_to_ty subs t, apply_subst_to_exp subs e)
  | ET.EUnitAbs e ->
      ET.EUnitAbs (apply_subst_to_exp subs e)
  | ET.EApp (e1, e2) ->
      ET.EApp (
        apply_subst_to_exp subs e1,
        apply_subst_to_exp subs e2
      )
  | ET.EPolyGen (e, pt) ->
      ET.EPolyGen (
        apply_subst_to_exp subs e,
        apply_subst_to_polyty subs pt
      )
  | ET.ELet (x, pt, e1, e2) ->
      ET.ELet (
        x,
        apply_subst_to_polyty subs pt,
        apply_subst_to_exp subs e1,
        apply_subst_to_exp subs e2
      )
  | ET.ERecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, e) -> (l, apply_subst_to_exp subs e))
      in ET.ERecord xs'
  | ET.ERecordGet (e, t, l) ->
      ET.ERecordGet (
        apply_subst_to_exp subs e,
        apply_subst_to_ty subs t,
        l
      )
  | ET.ERecordModify (e1, t, l, e2) ->
      ET.ERecordModify (
        apply_subst_to_exp subs e1,
        apply_subst_to_ty subs t,
        l,
        apply_subst_to_exp subs e2
      )
  | ET.ERecordAssign (e1, t, l, e2) ->
      ET.ERecordAssign (
        apply_subst_to_exp subs e1,
        apply_subst_to_ty subs t,
        l,
        apply_subst_to_exp subs e2
      )
  | ET.ERef e -> ET.ERef (apply_subst_to_exp subs e)
  | ET.EDeref e -> ET.EDeref (apply_subst_to_exp subs e)
  | ET.EStatement (e1, e2) ->
      ET.EStatement (apply_subst_to_exp subs e1, apply_subst_to_exp subs e2)
  | e -> e

(* S(k) *)
and apply_subst_to_kind subs = function
  | KUniv -> KUniv
  | KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, apply_subst_to_ty subs t))
      in KRecord xs'

(* S(tv) *)
and apply_subst_to_tyvar subs tv =
  List.fold_left (fun tv (tv1, t) ->
    match t with
      | TVar tv2 -> if tv = tv1 then tv2 else tv
      | _ -> tv
  ) tv subs

(* S(polyty) *)
and apply_subst_to_polyty subs = function
  | Forall (xs, t) ->
      let xs' =
        xs
        |> List.map (fun (tv, k) -> (tv, apply_subst_to_kind subs k))
      in Forall (xs', apply_subst_to_ty subs t)
