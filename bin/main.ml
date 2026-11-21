open CatML
open CatML
open CatML

(*
  inspired from:
  https://batsov.com/articles/2022/11/27/reading-files-in-ocaml/
*)
let read_lines (file : string) : string option =
  try
    let result =
      In_channel.with_open_bin file In_channel.input_all
      |> String.split_on_char '\n'
      |> List.fold_left (fun acc -> fun x -> if acc = "" then x else acc ^ " " ^ x) ""
    in
    Some result
  with
  | _ -> None

let () =
  let num_arguments = Array.length Sys.argv - 1 in
  match num_arguments with
  | 1 ->
    let file = Sys.argv.(1) in
    ( match read_lines file with
      | None -> prerr_endline ("Error: Unable to read file " ^ file)
      | Some contents ->
        ( match
            try Some (Main.parse contents) with
            | _ -> None
          with
          | None -> prerr_endline ("Error: Unable to parse contents of file " ^ file)
          | Some e ->
            ( match Lang.type_of_expr (Lang.empty_context, e) with
              | Error e' ->
                prerr_endline
                  ("Error: Sub-expression "
                   ^ Lang.string_of_expr e'
                   ^ " failed to typecheck"
                  )
              | Ok _ ->
                ( match Lang.eval e with
                  | Ok e' ->
                    print_endline ("Evaluation result:\n" ^ Lang.string_of_expr e')
                  | Error e' ->
                    (*should be impossible with types, but you never know!*)
                    prerr_endline "Error: Reached stuck expression. Full trace: ";
                    Lang.tracer e
                )
            )
        )
    )
  | _ -> prerr_endline ("Invalid number of arguments " ^ (num_arguments |> string_of_int))
