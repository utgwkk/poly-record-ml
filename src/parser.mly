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
%token UNIT
%token SEMI
%token REF BANG COLONEQ

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

FunExpr :
  FUN xs=nonempty_list(ID) RARROW e=Expr {
    List.fold_right (fun y e ->
      EAbs (y, e)
    ) xs e
  }
| FUN UNIT RARROW e=Expr { EUnitAbs e }

LetExpr :
  LET x=ID xs=list(ID) EQ e1=Expr IN e2=Expr {
    let e1 =
      List.fold_right (fun y e ->
        EAbs (y, e)
      ) xs e1
    in
    ELet (x, e1, e2)
  }
| LET x=ID UNIT EQ e1=Expr IN e2=Expr { ELet (x, EUnitAbs e1, e2) }
| ContinueExpr { $1 }

ContinueExpr :
  e1=AssignExpr SEMI e2=ContinueExpr { EStatement (e1, e2) }
| IfExpr { $1 }

IfExpr :
  IF e1=Expr THEN e2=Expr ELSE e3=Expr { EIfThenElse (e1, e2, e3) }
| AssignExpr { $1 }

AssignExpr :
  e1=LtExpr COLONEQ e2=AssignExpr { EBinOp (Assign, e1, e2) }
| LtExpr { $1 }

LtExpr :
  e1=PExpr LT e2=PExpr { EBinOp (Lt, e1,e2) }
| PExpr { $1 }

PExpr :
  e1=PExpr PLUS e2=MExpr { EBinOp (Plus, e1, e2) }
| MExpr { $1 }

MExpr :
  e1=MExpr MULT e2=AppExpr { EBinOp (Mult, e1, e2) }
| ConstructExpr { $1 }

ConstructExpr :
  REF e=AppExpr { ERef e }
| AppExpr { $1 }

AppExpr :
  e1=AppExpr e2=AExpr { EApp (e1, e2) }
| DerefExpr { $1 }

DerefExpr :
  BANG e=AExpr { EDeref e }
| AExpr { $1 }

AExpr :
  x=ID { EVar x }
| RecordExpr { $1 }
| INTV { EInt $1 }
| TRUE { EBool true }
| FALSE { EBool false }
| e=AExpr DOT l=ID { ERecordGet (e, l) } (* e.l *)
| MODIFY LPAREN e1=AExpr COMMA l=ID COMMA e2=Expr RPAREN { ERecordModify (e1, l, e2) }
| UNIT { EUnit }
| LPAREN Expr RPAREN { $2 }

RecordExpr : (* record constructor *)
  LRECORDPAREN separated_nonempty_list(COMMA, RecordField) RRECORDPAREN {
    let body = List.sort compare $2 in
    ERecord body
  }

RecordField :
  l=ID EQ e=Expr { (l, e) }
