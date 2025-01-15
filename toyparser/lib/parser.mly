%{
open Ast
%}

%token <string> CONST
%token <string> ENUM
%token PLUS
%token MINUS
%token DIV
%token MUL
%token LPAREN
%token RPAREN
%token EOF

%left MINUS PLUS
%left DIV MUL

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | n = ENUM { Const( Int32.of_string(n) |> Int32.to_int )}
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | MINUS; e1=expr{Sub(Const(0),e1)}
  | LPAREN; e=expr; RPAREN {e}
;

