type ('a, 'b) t = ('a * 'b) list

exception Not_bound

let empty = []

let extend x v env = (x, v) :: env

let rec lookup x env = try List.assoc x env with Not_found -> raise Not_bound

let rec lookup_opt x env = List.assoc_opt x env

let rec map f = function
  | [] -> []
  | (id, v) :: rest -> (id, f v) :: map f rest

let rec remove x = function
  | [] -> []
  | (id, v) :: rest -> if x = id then remove x rest else (id, v) :: remove x rest


let rec fold_right f env a =
  match env with [] -> a | (_, v) :: rest -> f v (fold_right f rest a)

let from_list xs =
  List.fold_left (fun env (k, v) -> extend k v env) xs empty

let domain env = List.map fst env
