(*
   CatML Abstract Syntax
   ----------------------
*)
type var = string

type expr =
  (* boolean expression forms *)
  | Bool of bool
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  (* arithmetic expression forms *)
  | Int of int
  | Plus of expr * expr
  | Mult of expr * expr
  | Equal of expr * expr
  (* pairs *)
  | First of expr
  | Second of expr
  | Pair of expr * expr
  (* conditional expression form *)
  | If of expr * expr * expr
  (* Variables and let expressions *)
  | Var of var
  | Let of var * expr * expr

(* TODO: Add pairs; functions; and recursive functions *)

(*
  is_val : expr -> bool
  in : AST [[e]]
  out : true iff e is a value
*)
let rec is_val e : bool =
  match e with
  | Int _ -> true
  | Bool _ -> true
  | Pair (e1, e2) -> is_val e1 && is_val e2
  | _ -> false

(*************************)
(* Operational Semantics *)
(*************************)

(* Define substitution *)
let rec subst (x, v, e) : expr =
  match e with
  | Bool _ | Int _ -> e
  | First e -> First (subst (x, v, e))
  | Second e -> Second (subst (x, v, e))
  | Var y -> if x = y then v else Var y
  | Not e' -> Not (subst (x, v, e'))
  | And (e1, e2) -> And (subst (x, v, e1), subst (x, v, e2))
  | Or (e1, e2) -> Or (subst (x, v, e1), subst (x, v, e2))
  | Plus (e1, e2) -> Plus (subst (x, v, e1), subst (x, v, e2))
  | Mult (e1, e2) -> Mult (subst (x, v, e1), subst (x, v, e2))
  | Equal (e1, e2) -> Equal (subst (x, v, e1), subst (x, v, e2))
  | Pair (e1, e2) -> Pair (subst (x, v, e1), subst (x, v, e2))
  | If (e0, e1, e2) -> If (subst (x, v, e0), subst (x, v, e1), subst (x, v, e2))
  | Let (y, e1, e2) ->
      Let (y, subst (x, v, e1), if x = y then e2 else subst (x, v, e2))

type step_result = Step of expr | NotReducible

(*
   step : expr -> step_result
   in : An expression [[e]]
   out : An expression [[e']] if e -> e' in the operational semantics;
         if no such [[e']] exists, returns [[NotReducible]]
*)
let rec step (e : expr) : step_result =
  match e with
  | Bool _ | Int _ | Var _ -> NotReducible
  (*cmp-not*)
  | Not (Bool b) -> Step (Bool (not b))
  (*ctx-not*)
  | Not e ->
      begin match step e with
      | Step e' -> Step (Not e')
      | NotReducible -> NotReducible
      end
  (*cmp-and*)
  | And (Bool b1, Bool b2) -> Step (Bool (b1 && b2))
  (*ctx-and*)
  | And (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (And (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (And (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  (*cmp-or*)
  | Or (Bool b1, Bool b2) -> Step (Bool (b1 || b2))
  (*ctx-or*)
  | Or (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Or (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Or (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  (*cmp-plus*)
  | Plus (Int n1, Int n2) -> Step (Int (n1 + n2))
  (*ctx-plus*)
  | Plus (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Plus (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Plus (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  (*cmp-mult*)
  | Mult (Int n1, Int n2) -> Step (Int (n1 * n2))
  (*ctx-mult*)
  | Mult (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Mult (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Mult (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  (*cmp-eq*)
  | Equal (Int n1, Int n2) -> Step (Bool (n1 = n2))
  (*ctx-eq*)
  | Equal (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Equal (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Equal (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  (*cmp-first*)
  | First (Pair (e1, e2)) -> Step e1
  (*ctx-first*)
  | First e -> (
      match step e with
      | Step e' -> Step (First e')
      | NotReducible -> NotReducible)
  (*cmp-second*)
  | Second (Pair (e1, e2)) -> Step e2
  (*ctx-second*)
  | Second e -> (
      match step e with
      | Step e' -> Step (Second e')
      | NotReducible -> NotReducible)
  (*ctx-pair*)
  | Pair (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Pair (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Pair (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  (*cmp-if-t*)
  | If (Bool true, e1, _) -> Step e1
  (*cmp-if-f*)
  | If (Bool false, _, e2) -> Step e2
  (*ctx-if*)
  | If (e1, e2, e3) ->
      begin match step e1 with
      | Step e1' -> Step (If (e1', e2, e3))
      | NotReducible -> NotReducible
      end
  (*cmp-let,ctx-let*)
  | Let (x, e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Let (x, e1', e2))
      | NotReducible ->
          if is_val e1 then Step (subst (x, e1, e2)) else NotReducible
      end

(*
   multistep : expr -> expr
   in : AST [[e]]
   out : [[e']] such that e ->* e' -/-> in the operational semantics
*)
let rec multistep e =
  match step e with Step e' -> multistep e' | NotReducible -> e

(*****************)
(* Type checking *)
(*****************)

type arith_type = INT | BOOL | PAIR of arith_type * arith_type
(* TODO: add pairs and function types *)

(* Define a type to represent finite maps/typing contexts *)
type context = (var * arith_type) list

(* empty_context : context *)
let empty_context : context = []

(* lookup : context * var -> arith_type option
 * input :
 *    g - a typing context
 *    x - a variable
 * output :
 *    if (x,tau) is in g, return Some tau
 *    otherwise, return None
 *)
let rec lookup (g, x) : arith_type option =
  match g with
  | [] -> None
  | (y, tp) :: g' -> if x = y then Some tp else lookup (g', x)

(* extend : context * var * arith_type -> context
 * input:
 *    g - a typing context
 *    x - a variable
 *    tp - an arith_type
 * output:
 *    A typing context that maps x to tp and any other y to g(y)
 *)
let extend (g, x, tp) : context = (x, tp) :: g

let rec type_of_expr (g, e) : arith_type option =
  match e with
  | Var x -> lookup (g, x)
  | Let (x, e1, e2) ->
      begin match type_of_expr (g, e1) with
      | None -> None
      | Some tp -> type_of_expr (extend (g, x, tp), e2)
      end
  | Bool _ -> Some BOOL
  | And (e1, e2) | Or (e1, e2) ->
      begin match (type_of_expr (g, e1), type_of_expr (g, e2)) with
      | Some BOOL, Some BOOL -> Some BOOL
      | _, _ -> None
      end
  | Not e' ->
      begin match type_of_expr (g, e') with Some BOOL -> Some BOOL | _ -> None
      end
  | Int _ -> Some INT
  | Plus (e1, e2) | Mult (e1, e2) ->
      begin match (type_of_expr (g, e1), type_of_expr (g, e2)) with
      | Some INT, Some INT -> Some INT
      | _, _ -> None
      end
  | Equal (e1, e2) ->
      begin match (type_of_expr (g, e1), type_of_expr (g, e2)) with
      | Some INT, Some INT -> Some BOOL
      | _, _ -> None
      end
  | First e ->
      begin match type_of_expr (g, e) with
      | Some (PAIR (t1, t2)) -> Some t1
      | _ -> None
      end
  | Second e ->
      begin match type_of_expr (g, e) with
      | Some (PAIR (t1, t2)) -> Some t2
      | _ -> None
      end
  | Pair (e1, e2) ->
      begin match (type_of_expr (g, e1), type_of_expr (g, e2)) with
      | Some t1, Some t2 -> Some (PAIR (t1, t2))
      | _, _ -> None
      end
  | If (e0, e1, e2) ->
      begin match
        (type_of_expr (g, e0), type_of_expr (g, e1), type_of_expr (g, e2))
      with
      | Some BOOL, Some t1, Some t2 -> if t1 = t2 then Some t1 else None
      | _, _, _ -> None
      end

(********************************************)
(* Utility functions for working with ARITH *)
(********************************************)

(*
  string_of_expr : expr -> string
  in : An expression AST [[e]].
  out : The concrete expression e in string format.
*)
let rec string_of_expr e =
  match e with
  (* TODO: extend string_of_expr *)
  | Var x -> x
  | Let (x, e1, e2) ->
      "Let " ^ x ^ " = " ^ string_of_expr e1 ^ " In " ^ string_of_expr e2
  | Bool true -> "True"
  | Bool false -> "False"
  | Int n -> string_of_int n
  | And (e1, e2) -> "(" ^ string_of_expr e1 ^ " And " ^ string_of_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_expr e1 ^ " Or " ^ string_of_expr e2 ^ ")"
  | Not e1 -> "(Not " ^ string_of_expr e1 ^ ")"
  | Plus (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Mult (e1, e2) -> "(" ^ string_of_expr e1 ^ " x " ^ string_of_expr e2 ^ ")"
  | Equal (e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ")"
  | First e -> "(First " ^ string_of_expr e ^ ")"
  | Second e -> "Second " ^ string_of_expr e
  | Pair (e1, e2) -> "(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | If (e1, e2, e3) ->
      "If " ^ string_of_expr e1 ^ " Then " ^ string_of_expr e2 ^ " Else "
      ^ string_of_expr e3

(*
  pretty_trace : expr -> bool -> unit
  in : AST [[e]], stepper flag
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke
  between reductions if stepper is on.
*)
let rec pretty_trace e stepper =
  Printf.printf "%s" (string_of_expr e);
  if stepper then ignore (read_line ()) else ();
  if is_val e then (
    Printf.printf "\n";
    flush stdout)
  else
    match step e with
    | NotReducible ->
        Printf.printf "  (Bad, Stuck Expression)\n";
        flush stdout
    | Step e' ->
        Printf.printf "->\n";
        flush stdout;
        pretty_trace e' stepper

(*
  stepper : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke
  between reductions.
*)
let rec stepper e = pretty_trace e true

(*
  tracer : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e.
*)
let rec tracer e = pretty_trace e false
