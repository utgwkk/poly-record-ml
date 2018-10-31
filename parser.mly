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

%start main
%type <Syntax.exp> main
%%

main:
  Expr SEMISEMI { $1 }

Expr:
  LetExpr { $1 }

LetExpr:
  LET x=ID EQ e1=Expr IN e2=Expr { Let (x, e1, e2) }
| AExpr { $1 }

AExpr:
  ID { Var $1 }
| INT { Int $1 }
| LMPAREN rb=RecordBody RMPAREN { Record rb }
| LPAREN e=Expr RPAREN { e }

RecordBody:
| k=ID EQ e=Expr COMMA r=RecordBody { (k, e) :: r }
| k=ID EQ e=Expr { [(k, e)] }
  
