%{
  open Syntax
%}

%token SEMISEMI
%token <Syntax.id> ID
%token <int> INT
%token LPAREN RPAREN
%token LET EQ IN

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
| LPAREN e=Expr RPAREN { e }
  
