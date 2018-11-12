open Syntax
open PolyRecord
module ET = ExplicitlyTyped

(* E *)
type eqs = (ty * ty) list

(* S *)
type 'a subst = (tyvar * 'a) list

(* ty subst -> eqs *)
let eqs_of_subst s = List.map (fun (tv, a) -> (TVar tv, a)) s

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
let apply_subst_to_ty subst t = List.fold_left subst_ty t subst

(* S(T) *)
let apply_subst_to_tyenv s tyenv =
  List.fold_left (fun env s -> Environment.map (fun (Forall (xs, t)) -> Forall (xs, subst_ty t s)) env) tyenv s

(* [ty/tyvar]E *)
let subst_eqs subst eqs =
  List.map (fun (t1, t2) -> (apply_subst_to_ty subst t1, apply_subst_to_ty subst t2)) eqs

(* レコードのフィールド名の和集合をとる *)
let union_record xs ys =
  let xs' =
    xs
    |> List.map fst
    |> List.sort compare
  in
  let ys' =
    ys
    |> List.map fst
    |> List.sort compare
  in
  let rec inner xs ys = match xs with
    | [] -> ys
    | lx :: tx -> match ys with
        | [] -> xs
        | ly :: ty ->
            if lx = ly then lx :: inner tx ty
            else if lx > ly then ly :: inner (lx :: tx) ty
            else lx :: inner tx (ly :: ty)
  in inner xs' ys'

(* レコードのフィールド名の積集合をとる *)
let intersect_record xs ys =
  let xs' =
    xs
    |> List.map fst
    |> List.sort compare
  in
  let ys' =
    ys
    |> List.map fst
    |> List.sort compare
  in
  let rec inner xs ys = match xs with
    | [] -> []
    | lx :: tx -> match ys with
        | [] -> []
        | ly :: ty ->
            if lx = ly then lx :: inner tx ty
            else if lx > ly then inner (lx :: tx) ty
            else inner tx (ly :: ty)
  in inner xs' ys'

(* dom(F1) \subseteq dom(F2) *)
let dom_is_subset xs ys =
  let xs' = List.map fst xs in
  let ys' = List.map fst ys in
  List.for_all (fun x -> List.mem x ys') xs'

(* dom(F1) = dom(F2) *)
let dom_eq xs ys =
  let xs' = List.map fst xs in
  let ys' = List.map fst ys in
  List.for_all (fun x -> List.mem x ys') xs' &&
  List.for_all (fun y -> List.mem y xs') ys'

(* tyvar \in FTV(ty) *)
let rec ftv tv = function
  | TVar tv' -> tv = tv'
  | TFun (t1, t2) -> ftv tv t1 || ftv tv t2
  | TRecord xs -> List.exists (fun (_, t) -> ftv tv t) xs
  | _ -> false

exception Unification_failed of string

(* \mathcal{U} *)
let rec unify eqs kenv subst ksubst =
  match eqs with
  | [] -> (eqs, kenv, subst, ksubst)
  | (t1, t2) :: rest ->
      if ty_eq t1 t2 then unify rest kenv subst ksubst (* (I) *)
      else match t1, t2 with
      | TVar tv1, TVar tv2 ->
          let k1 = Environment.lookup tv1 kenv in
          let k2 = Environment.lookup tv2 kenv in
          begin match k1, k2 with
          | KRecord xs, KRecord ys -> (* (III) *)
              let isect = intersect_record xs ys in
              let constrs = List.map (fun x -> (List.assoc x xs, List.assoc x ys)) isect in
              let eqs' = constrs @ subst_eqs [(tv1, TVar tv2)] rest in
              let uni = union_record xs ys in
              let k' =
                KRecord (List.map (fun x -> (x, List.assoc x (xs @ ys))) uni)
                |> subst_kind (tv1, TVar tv2)
              in
              let kenv' =
                kenv
                |> subst_kenv (tv1, TVar tv2)
                |> Environment.extend tv2 k'
              in
              let subst' = (tv1, TVar tv2) :: subst_subs (tv1, TVar tv2) subst in
              let ksubst' =
                ksubst
                |> subst_kenv (tv1, TVar tv2)
                |> Environment.extend tv1 (KRecord xs)
              in
              (eqs', kenv', subst', ksubst')
          | KUniv, KUniv -> (* (II)' *)
              let eqs' = subst_eqs [(tv1, TVar tv2)] rest in
              let kenv' = subst_kenv (tv1, TVar tv2) kenv in
              let subst' = (tv1, TVar tv2) :: subst_subs (tv1, TVar tv2) subst in
              let ksubst' = subst_kenv (tv1, TVar tv2) ksubst in
              (eqs', kenv', subst', ksubst')
          | _ -> raise (Unification_failed "not same record")
          end
      | TVar tv, TRecord ys -> (* (IV) *)
          let k = Environment.lookup tv kenv in
          begin match k with
          | KRecord xs ->
              let eqs' =
                List.map (fun (l, _) -> (List.assoc l xs, List.assoc l ys)) xs @ rest
                |> subst_eqs [(tv, TRecord ys)]
              in
              let kenv' = subst_kenv (tv, TRecord ys) kenv in
              let subst' = (tv, TRecord ys) :: subst_subs (tv, TRecord ys) subst in
              let ksubst' =
                kenv
                |> subst_kenv (tv, TRecord ys)
                |> Environment.extend tv (KRecord xs)
              in
              if dom_is_subset xs ys && not (ftv tv (TRecord xs))
              then (eqs', kenv', subst', ksubst')
              else raise (Unification_failed "not a subset record")
          | _ -> raise (Unification_failed "not a record")
          end
      | TVar tv, t -> (* (II) *)
          if ftv tv t then raise (Unification_failed "free variable occurence")
          else
            let eqs' = subst_eqs [(tv, t)] rest in
            let kenv' = subst_kenv (tv, t) kenv in
            let subst' = subst_subs (tv, t) subst in
            let ksubst' = subst_kenv (tv, t) ksubst in
            (eqs', kenv', subst', ksubst')
      | TRecord xs, TRecord ys -> (* (V) *)
          if dom_eq xs ys then
            let eqs' =
              List.map (fun (l, _) -> (List.assoc l xs, List.assoc l ys)) xs @ rest
            in
            (eqs', kenv, subst, ksubst)
          else raise (Unification_failed "not the same domain record")
      | TFun (t11, t12), TFun (t21, t22) -> (* (IX) *)
          let eqs' = (t11, t21) :: (t12, t22) :: rest in
          (eqs', kenv, subst, ksubst)
      | t, TVar tv -> (* (II) *)
          let eqs' = (TVar tv, t) :: rest in
          unify eqs' kenv subst ksubst
      | _ -> raise (Unification_failed "unify failed")
