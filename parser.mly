%{
  open Syntax
%}

%token SEMISEMI
%token <Syntax.id> ID
%token <int> INT
%token LPAREN RPAREN
%token LET EQ IN
%token LMPAREN RMPAREN (* { } *)
%token COMMA
%token DOT
%token PLUS MULT LT
%token FUN RARROW (* -> *)
%token TRUE FALSE
%token MODIFY
%token IF THEN ELSE

%start main
%type <Syntax.exp> main
%%

main:
  Expr SEMISEMI { $1 }

Expr:
  LetExpr { $1 }

LetExpr:
  LET x=ID EQ e1=Expr IN e2=Expr { Let (x, e1, e2) }
| IfExpr { $1 }

IfExpr:
  IF e1=Expr THEN e2=Expr ELSE e3=Expr { IfThenElse (e1, e2, e3) }
| FunExpr { $1 }

FunExpr:
  FUN x=ID RARROW e=Expr { Fun (x, e) }
| BinOpExpr { $1 }

BinOpExpr:
  LTExpr { $1 }

LTExpr:
  e1=PlusExpr LT e2=PlusExpr { BinOp (Lt, e1, e2) }
| PlusExpr { $1 }

PlusExpr:
  e1=PlusExpr PLUS e2=MultExpr { BinOp (Plus, e1, e2)}
| MultExpr { $1 }

MultExpr:
  e1=MultExpr MULT e2=AExpr { BinOp (Mult, e1, e2) }
| AppExpr { $1 }

AppExpr:
  e1=AppExpr e2=AExpr { App (e1, e2) }
| AExpr { $1 }

AExpr:
  ID { Var $1 }
| INT { Int $1 }
| TRUE { Bool true }
| FALSE { Bool false }
| e=AExpr DOT f=ID { RecordGet (f, e) }
| LMPAREN rb=RecordBody RMPAREN { Record rb }
| MODIFY LPAREN e1=Expr COMMA f=ID COMMA e2=Expr RPAREN { RecordModify (e1, f, e2) }
| LPAREN e=Expr RPAREN { e }

RecordBody:
| k=ID EQ e=Expr COMMA r=RecordBody { (k, e) :: r }
| k=ID EQ e=Expr { [(k, e)] }
  
