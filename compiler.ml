module Lld = Lambda_let_dot
module Llp = Lambda_let_paren

let idxset_kind tv = function
  | Lld.KUniv -> []
  | Lld.KRecord xs -> List.map (fun (l, _) -> (l, Llp.TVar tv)) xs

let idxset xs =
  let rec inner = function
  | [] -> []
  | (tv, k) :: tl -> idxset_kind tv k :: inner tl
  in inner xs |> List.flatten

let rec monotycon = function
  | Lld.TVar tv -> Llp.TVar tv
  | Lld.TInt -> Llp.TInt
  | Lld.TFun (t1, t2) -> Llp.TFun (monotycon t1, monotycon t2)
  | Lld.TRecord ts ->
      let ts' = List.map (fun (l, t) -> (l, monotycon t)) ts in
      Llp.TRecord ts'

let kcon = function
  | Lld.KUniv -> Llp.KUniv
  | Lld.KRecord xs ->
      Llp.KRecord (List.map (fun (l, t) -> (l, monotycon t)) xs)

let rec tycon (Lld.Forall (xs, t)) =
  let idxsets = idxset xs in
  let xs' = List.map (fun (tv, k) -> (tv, kcon k)) xs in
  match idxsets with
  | [] -> Llp.Forall (xs', monotycon t)
  | _ -> Llp.Forall (xs', Llp.TIdxFun (idxsets, monotycon t))

let rec compile (lbenv : Lld.lbenv) (tyenv : Lld.tyenv) (exp : Lld.exp) = Llp.EInt 1

(* entrypoint *)
let start exp = compile Environment.empty Environment.empty exp
