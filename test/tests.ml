open CatML

let print_test (contents, expected) =
  let result = Main.parse contents |> Lang.multistep |> Lang.string_of_expr in
  if result = expected
  then ()
  else
    print_endline
      ("Test string " ^ contents ^ " produced result " ^ result ^ ", expected " ^ expected);
  ()

let () =
  print_test ("1 + 2", "3");
  (* precedence *)
  print_test ("1 + 2 * 3", "7");
  print_test ("If False Or Not False Then -28 Else 13", "-28");
  print_test ("First (1, 2)", "1");
  ()
