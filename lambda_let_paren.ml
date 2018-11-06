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
  | EIdxAbs of idx * exp
  | EIdxApp of exp * idx

type value =
  | VInt of int
  | VProc of id * exp * env
  | VArray of value array
  | VIdxAbs of idx * exp

and env = (id, value) Environment.t
