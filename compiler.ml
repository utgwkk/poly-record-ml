module ET = ExplicitlyTyped
module Impl = Implementation

(* IdxSet(tv::k) *)
let idxset_kind tv = function
  | ET.KUniv -> []
  | ET.KRecord xs ->
      List.map (fun (l, _) -> (l, Impl.TVar tv)) xs

(* IdxSet(F) where k = {{F}}*)
let idxset xs =
  let rec inner = function
  | [] -> []
  | (tv, k) :: tl -> idxset_kind tv k :: inner tl
  in inner xs |> List.flatten

(* (\tau)^* = \tau *)
let rec monotycon = function
  | ET.TVar tv -> Impl.TVar tv
  | ET.TInt -> Impl.TInt
  | ET.TBool -> Impl.TBool
  | ET.TFun (t1, t2) -> Impl.TFun (monotycon t1, monotycon t2)
  | ET.TRecord ts ->
      let ts' =
        ts
        |> List.map (fun (l, t) -> (l, monotycon t))
        |> List.sort compare
      in
      Impl.TRecord ts'

let kcon = function
  | ET.KUniv -> Impl.KUniv
  | ET.KRecord xs ->
      let xs' =
        xs
        |> List.map (fun (l, t) -> (l, monotycon t))
      in
      Impl.KRecord xs'

(* (forall t_1::k_1. \cdots forall t_n::k_n.\tau)^*
 * = forall t_1::k_1. \cdots forall t_n::k_n.
 *   idx(l_1, t^'_1) => ... => idx(l_n, t^'_n) => \tau
 * *)
let rec tycon (ET.Forall (xs, t)) =
  let idxsets = idxset xs in
  let xs' =
    xs
    |> List.map (fun (tv, k) -> (tv, kcon k))
  in
  match idxsets with
  | [] -> Impl.Forall (xs', monotycon t)
  | _ -> Impl.Forall (xs', Impl.TIdxFun (idxsets, monotycon t))

(* counter for fresh index variables *)
let __counter = ref 1

let reset_counter () =
  __counter := 1

let fresh_idxvar () =
  let rval = !__counter in
  __counter := !__counter + 1;
  rval

let rec compile (lbenv : Impl.lbenv) tyenv = function
  | ET.EPolyInst (x, xs) ->
      let (Impl.Forall (ys, t)) = Environment.lookup x tyenv in
      begin match t with
        | Impl.TIdxFun (zs, _) ->
            let subs =
              List.map2 (fun (tv, _) ty -> (tv, monotycon ty)) ys xs
            in
            let idxs = List.map (fun (l, t) ->
              let t' = Impl.substitute subs t in
              try Impl.INat (Impl.idx_value l t')
              with Impl.Undefined_index_value ->
                Environment.lookup (l, t') lbenv
            ) zs
            in
            idxs
            |> List.fold_left (fun e idx -> Impl.EIdxApp (e, idx)) (Impl.EVar x)
        | _ ->
            (* Since there is no polymorphic instantiation
             * just convert to a variable expression
             * *)
            Impl.EVar x
      end
  | ET.EInt i -> Impl.EInt i
  | ET.EBool b -> Impl.EBool b
  | ET.EBinOp (op, e1, e2) ->
      let e1' = compile lbenv tyenv e1 in
      let e2' = compile lbenv tyenv e2 in
      Impl.EBinOp (op, e1', e2')
  | ET.EIfThenElse (e1, e2, e3) ->
      let e1' = compile lbenv tyenv e1 in
      let e2' = compile lbenv tyenv e2 in
      let e3' = compile lbenv tyenv e3 in
      Impl.EIfThenElse (e1', e2', e3')
  | ET.EAbs (x, t, e) ->
      let t' = Impl.Forall ([], monotycon t) in
      let tyenv' = Environment.extend x t' tyenv in
      let e' = compile lbenv tyenv' e in
      Impl.EAbs (x, e')
  | ET.EApp (e1, e2) ->
      let e1' = compile lbenv tyenv e1 in
      let e2' = compile lbenv tyenv e2 in
      Impl.EApp (e1', e2')
  | ET.EPolyGen (e, (ET.Forall (xs, _))) ->
      let xs' = idxset xs in
      let label_tv_idxvar_tuple_list =
        List.map (fun (l, t) -> (l, t, fresh_idxvar ())) xs'
      in
      let lbenv' =
        List.fold_left
        (fun env (l, t, i) -> Environment.extend (l, t) (Impl.IVar i) env)
        lbenv label_tv_idxvar_tuple_list
      in
      let e' = compile lbenv' tyenv e in
      let fresh_idxvars =
        List.rev_map (fun (_, _, i) -> i) label_tv_idxvar_tuple_list
      in
      fresh_idxvars
      |> List.fold_left (fun e idxv -> Impl.EIdxAbs (idxv, e)) e'
  | ET.ELet (x, pt, e1, e2) ->
      let pt' = tycon pt in
      let e1' = compile lbenv tyenv e1 in
      let tyenv' = Environment.extend x pt' tyenv in
      let e2' = compile lbenv tyenv' e2 in
      Impl.ELet (x, e1', e2')
  | ET.ERecord xs ->
      let xs' =
        xs
        |> List.sort compare
        |> List.map (fun (_, e) -> compile lbenv tyenv e)
      in
      Impl.EArray xs'
  | ET.ERecordGet (e1, t, l) ->
      let e1' = compile lbenv tyenv e1 in
      let t' = monotycon t in
      let idx =
        try Impl.INat (Impl.idx_value l t')
        with Impl.Undefined_index_value ->
          Environment.lookup (l, t') lbenv
      in Impl.EArrayGet (e1', idx)
  | ET.ERecordModify (e1, t, l, e2) ->
      let e1' = compile lbenv tyenv e1 in
      let t' = monotycon t in
      let idx =
        try Impl.INat (Impl.idx_value l t')
        with Impl.Undefined_index_value ->
          Environment.lookup (l, t') lbenv
      in
      let e2' = compile lbenv tyenv e2 in
      Impl.EArrayModify (e1', idx, e2')

(* entrypoint *)
let start exp =
  reset_counter ();
  compile Environment.empty Environment.empty exp
