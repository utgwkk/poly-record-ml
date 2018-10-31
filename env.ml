type 'a t = (Syntax.id * 'a) list

exception Not_bound of Syntax.id

let empty = []

let extend id item env = (id, item) :: env

let lookup id env =
  try List.assoc id env
  with Not_found -> raise (Not_bound id)
