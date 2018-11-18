open Syntax
open PolyRecord
open Subst

(* E *)
type eqs = (ty * ty) list

(* ty subst -> eqs *)
let eqs_of_subst s = List.map (fun (tv, a) -> (TVar tv, a)) s

(* レコードのフィールド名の和集合をとる *)
let union_record xs ys =
  let xs' =
    xs
    |> List.map fst
  in
  let ys' =
    ys
    |> List.map fst
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
  in
  let ys' =
    ys
    |> List.map fst
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
  try List.fold_left2 (fun b x y -> b && x = y) true xs' ys'
  with _ -> false

exception Unification_failed of string

(* \mathcal{U} *)
let rec unify eqs kenv subst ksubst =
  match eqs with
  | [] -> (kenv, subst)
  | (t1, t2) :: rest ->
      if t1 = t2 then unify rest kenv subst ksubst (* (I) *)
      else match t1, t2 with
      | TRecord xs, TRecord ys -> (* (V) *)
          if dom_eq xs ys then
            let eqs' =
              List.map (fun (l, _) -> (List.assoc l xs, List.assoc l ys)) xs @ rest
            in
            unify eqs' kenv subst ksubst
          else raise (Unification_failed "not the same domain record")
      | TFun (t11, t12), TFun (t21, t22) -> (* (IX) *)
          let eqs' = (t11, t21) :: (t12, t22) :: rest in
          unify eqs' kenv subst ksubst
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
              unify eqs' kenv' subst' ksubst'
          | KRecord _, KUniv -> (* (II)' *)
              unify ((TVar tv2, TVar tv1):: rest) kenv subst ksubst
          | KUniv, _ -> (* (II)' *)
              let eqs' = subst_eqs [(tv1, TVar tv2)] rest in
              let kenv' =
                kenv
                |> subst_kenv (tv1, TVar tv2)
              in
              let subst' = (tv1, TVar tv2) :: subst_subs (tv1, TVar tv2) subst in
              let ksubst' =
                ksubst
                |> subst_kenv (tv1, TVar tv2)
                |> Environment.extend tv1 KUniv
              in
              unify eqs' kenv' subst' ksubst'
          end
      | TVar tv, TRecord ys -> (* (IV) *)
          let k = Environment.lookup tv kenv in
          begin match k with
          | KUniv ->
              let eqs' = subst_eqs [(tv, TRecord ys)] rest in
              let kenv' = subst_kenv (tv, TRecord ys) kenv in
              let subst' = (tv, TRecord ys) :: subst_subs (tv, TRecord ys) subst in
              let ksubst' =
                kenv
                |> subst_kenv (tv, TRecord ys)
                |> Environment.extend tv (KRecord ys)
              in
              unify eqs' kenv' subst' ksubst'
          | KRecord xs ->
              let eqs' =
                List.map (fun (l, k) ->
                  try (k, List.assoc l ys)
                  with Not_found ->
                    raise (Unification_failed (Printf.sprintf "field '%s' not found on %s" l (string_of_kind (KRecord ys))))
                ) xs @ rest
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
              then unify eqs' kenv' subst' ksubst'
              else raise (Unification_failed "not a subset record")
          end
      | TVar tv, t -> (* (II) *)
          if ftv tv t then
            raise (Unification_failed (Printf.sprintf "free variable %d occurs inside %s" tv (string_of_ty t)))
          else
            let k = Environment.lookup tv kenv in
            begin match k with
                | KUniv ->
                    let eqs' = subst_eqs [(tv, t)] rest in
                    let kenv' = subst_kenv (tv, t) kenv in
                    let subst' = (tv, t) :: subst_subs (tv, t) subst in
                    let ksubst' = subst_kenv (tv, t) ksubst in
                    unify eqs' kenv' subst' ksubst'
                | _ -> raise (Unification_failed "unify failed")
            end
      | t, TVar tv -> (* (II) *)
          let eqs' = (TVar tv, t) :: rest in
          unify eqs' kenv subst ksubst
      | _ -> (* failure *)
          let error_msg = match t1, t2 with
              | _, TFun _ -> Printf.sprintf "%s is not a function type" (string_of_ty t1)
              | _ -> Printf.sprintf "type mismatch (%s, %s)" (string_of_ty t1) (string_of_ty t2)
          in raise (Unification_failed error_msg)

let start eqs kenv = unify eqs kenv [] Environment.empty
