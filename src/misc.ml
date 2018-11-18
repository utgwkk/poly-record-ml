(* apply f until the return value is fixed *)
let rec until_fix old f =
  let next = f old in
  if old = next then old
  else until_fix next f
