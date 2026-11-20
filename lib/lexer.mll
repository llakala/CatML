{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z' '_' '\'']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "True" { TRUE }
  | "False" { FALSE }
  | "*" { TIMES }
  | "+" { PLUS }
  | "And" { AND }
  | "Or" { OR }
  | "Not" { NOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "Let" { LET }
  | "=" { EQUALS }
  | "In" { IN }
  | "If" { IF }
  | "Then" { THEN }
  | "Else" { ELSE }
  | "Fun" { FUN }
  | "Fix" { FIX }
  | "First" { FIRST }
  | "Second" { SECOND }
  | ":" { COLON }
  | "," { COMMA }
  | "->" { ARROW }
  | "Int" { INTTYPE }
  | "Bool" { BOOLTYPE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
