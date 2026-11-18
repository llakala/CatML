%{
open Lang
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token OR
%token AND
%token NOT
%token PLUS
%token TIMES
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

%nonassoc IN
%nonassoc ELSE
%left EQUALS
%left PLUS
%left TIMES
%left OR
%left AND
%left NOT

%start <Lang.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i    = INT { Int i }
  | x    = ID  { Var x }
  | TRUE       { Bool true }
  | FALSE      { Bool false }
  | e1 = expr; EQUALS; e2 = expr { Equal (e1, e2) }
  | e1 = expr; TIMES;  e2 = expr { Mult(e1, e2) }
  | e1 = expr; PLUS;   e2 = expr { Plus(e1, e2) }
  | e1 = expr; AND;  e2 = expr { And(e1, e2) }
  | e1 = expr; OR;   e2 = expr { Or(e1, e2) }
  | NOT; e = expr { Not(e) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr   { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  ;
