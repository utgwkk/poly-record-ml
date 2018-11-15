%{
  open Syntax
  open PolyRecord
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token LET EQ IN
%token IF THEN ELSE TRUE FALSE
%token RARROW FUN
%token LRECORDPAREN RRECORDPAREN (* { } *)
%token DOT MODIFY COMMA

%token <int> INTV
%token <Syntax.id> ID

%start main
%type <PolyRecord.exp> main
%%

(* expressions *)

main :
  Expr SEMISEMI { $1 }

Expr :
  LetExpr { $1 }
| FunExpr { $1 }
| IfExpr { $1 }
| LtExpr { $1 }

FunExpr :
  FUN x=ID RARROW e=Expr { EAbs (x, e)}

LetExpr :
  LET x=ID EQ e1=Expr IN e2=Expr { ELet (x, e1, e2) }

IfExpr :
  IF e1=Expr THEN e2=Expr ELSE e3=Expr { EIfThenElse (e1, e2, e3) }

LtExpr :
  e1=PExpr LT e2=PExpr { EBinOp (Lt, e1,e2) }
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
  x=ID { EVar x }
| RecordExpr { $1 }
| INTV { EInt $1 }
| TRUE { EBool true }
| FALSE { EBool false }
| e=AExpr DOT l=ID { ERecordGet (e, l) } (* e.l *)
| MODIFY LPAREN e1=AExpr COMMA l=ID COMMA e2=Expr RPAREN { ERecordModify (e1, l, e2) }
| LPAREN Expr RPAREN { $2 }

RecordExpr :
  LRECORDPAREN separated_nonempty_list(COMMA, RecordField) RRECORDPAREN { ERecord $2 } (* record constructor *)

RecordField :
  l=ID EQ e=Expr { (l, e) }
