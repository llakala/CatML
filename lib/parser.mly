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
%token FIRST
%token SECOND
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token FIX
%token COLON
%token COMMA
%token ARROW
%token EOF

%token INTTYPE
%token BOOLTYPE

%nonassoc IN
%left ELSE
%left EQUALS
%left PLUS
%left TIMES
%left OR
%left AND
%left NOT
%nonassoc ID INT LPAREN TRUE FALSE LET FUN FIX IF FIRST SECOND COMMA
%right ARROW
%nonassoc apply

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
  | FUN; x = ID; COLON; t = expr_type; EQUALS; e = expr; { Fun (x, t, e) }
  | FIX; e = expr; { Fix (e) }
  | e1 = expr; e2 = expr; %prec apply { Apply(e1, e2) }
  | FIRST; e = expr { First(e) }
  | SECOND; e = expr { Second(e) }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair(e1, e2) }
  | LPAREN; e = expr; RPAREN {e}
  ;

expr_type:
  | INTTYPE { INT }
  | BOOLTYPE { BOOL }
  | t1 = expr_type; ARROW; t2 = expr_type { FUN(t1, t2) }
  | t1 = expr_type; COMMA; t2 = expr_type { PAIR(t1, t2) }
  | LPAREN; t = expr_type; RPAREN { t }
