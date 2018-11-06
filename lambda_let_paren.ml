type id = Syntax.id

type idxvar = int

type idx =
  | IVar of idxvar
  | INat of int

type exp =
  | EVar of id
  | EInt of int
  | EAbs of id * exp
  | EApp of exp * exp
  | ELet of id * exp * exp
  | EArray of exp list
  | EArrayGet of exp * idx
  | EArrayModify of exp * idx * exp
  | EIdxAbs of idxvar * exp
  | EIdxApp of exp * idx

type value =
  | VInt of int
  | VProc of id * exp * env
  | VArray of value array
  | VIdxAbs of idxvar * exp

and env = (id, value) Environment.t

and idxenv = (idxvar, idx) Environment.t
