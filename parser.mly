%{
  open Syntax
  open ExplicitlyTyped
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token LET EQ IN
%token IF THEN ELSE TRUE FALSE
%token RARROW FUN
%token LRECORDPAREN RRECORDPAREN (* { } *)
%token POLY
%token DOT MODIFY COMMA

%token <int> INTV
%token <Syntax.id> ID

(* Type specifier *)
%token COLON
%token INT BOOL
%token <Syntax.tyvar> TVAR
%token FORALL COLONCOLON
%token KLPAREN KRPAREN (* {{ }} *)

%start main
%type <ExplicitlyTyped.exp> main
%%

(* monotypes *)
Type :
  FunType { $1 }

FunType :
  t1=AType RARROW t2=FunType { TFun (t1, t2) }
| AType { $1 }

AType :
  INT { TInt }
| BOOL { TBool }
| TVAR { TVar $1 }
| LRECORDPAREN TypeRecordBody RRECORDPAREN { TRecord $2 }
| LPAREN Type RPAREN { $2 }

TypeRecordBody :
  l=ID COLON t=Type COMMA r=TypeRecordBody { (l, t) :: r }
| l=ID COLON t=Type { [(l, t)] }

(* kinds *)
Kind :
  KLPAREN KindRecordBody KRPAREN { KRecord $2 }

KindRecordBody :
  l=ID COLON t=Type COMMA r=KindRecordBody { (l, t) :: r }
| l=ID COLON t=Type { [(l, t)] }

(* polytypes *)
PolyType :
  Type { Forall ([], $1) }
| FORALL bs=BoundBody t=Type { Forall (bs, t) }

BoundBody :
  bp=BoundPart DOT r=BoundBody { bp :: r }
| bp=BoundPart DOT { [bp] }

BoundPart :
  tv=TVAR COLONCOLON k=Kind { (tv, k) }
| tv=TVAR { (tv, KUniv) }

(* expressions *)

main :
  Expr SEMISEMI { $1 }

Expr :
  LetExpr { $1 }
| FunExpr { $1 }

FunExpr :
  FUN LPAREN x=ID COLON t=Type RPAREN RARROW e=Expr { EAbs (x, t, e)}

LetExpr :
  LET x=ID COLON pt=PolyType EQ e1=Expr IN e2=Expr { ELet (x, pt, e1, e2) }
| IfExpr { $1 }

IfExpr :
  IF e1=Expr THEN e2=Expr ELSE e3=Expr { EIfThenElse (e1, e2, e3) }
| LtExpr { $1 }

LtExpr :
  e1=IfExpr LT e2=IfExpr { EBinOp (Lt, e1,e2) }
| PExpr { $1 }

PExpr :
  e1=PExpr PLUS e2=MExpr { EBinOp (Plus, e1, e2) }
| MExpr { $1 }

MExpr :
  e1=MExpr MULT e2=AppExpr { EBinOp (Mult, e1, e2) }
| AppExpr { $1 }

AppExpr :
  e1=AppExpr e2=AExpr { EApp (e1, e2) }
| AExpr { $1 }

AExpr :
  x=ID is=PolyInstBody { EPolyInst (x, is) }
| x=ID { EPolyInst (x, []) }
| INTV { EInt $1 }
| TRUE { EBool true }
| FALSE { EBool false }
| LRECORDPAREN RecordBody RRECORDPAREN { ERecord $2 } (* record constructor *)
| e=AExpr COLON t=Type DOT l=ID { ERecordGet (e, t, l) } (* e:t.l *)
| MODIFY LPAREN e1=AExpr COLON t=Type COMMA l=ID COMMA e2=Expr RPAREN { ERecordModify (e1, t, l, e2) }
| POLY LPAREN e=AExpr COLON pt=PolyType RPAREN { EPolyGen (e, pt) }
| LPAREN Expr RPAREN { $2 }

PolyInstBody :
  t=Type r=PolyInstBody { t :: r }
| t=Type { [t] }

RecordBody :
  l=ID EQ e=Expr COMMA r=RecordBody { (l, e) :: r }
| l=ID EQ e=Expr { [(l, e)] }
