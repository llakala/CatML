open CatML
open CatML.Lang

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let () = parse "Let x = True And True In x And x" |> tracer
