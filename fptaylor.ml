(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Lib
open List
open Parser
open Expr
open Expr_sym

let exprs () = Environment.env.Environment.expressions

let gradient e =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report "Derivatives: " in
  let p = print_string in
  let vs = vars_in_expr e in
  let ds' = map (fun v -> diff v e) vs in
  let ds = map Maxima.simplify ds' in
  let dsm = map (fun v -> Maxima.simplify_diff v e) vs in
  let _ = map (fun (v, d) ->
    p v; p ": "; 
    print_expr_std d; nl ()) (zip vs ds) in
  let _ = report "Maxima diff: " in
  let _ = map (fun (v, d) ->
    p v; p ": ";
    print_expr_std d; nl ()) (zip vs dsm) in
  let dd = map2 (fun d1 d2 -> 
    Maxima.simplify (mk_sub {op_exact=false} d1 d2)) ds dsm in
  let _ = map (fun d -> 
    if not (eq_expr d expr_0) then failwith "Unequal derivatives" else ()) dd in
  ()

let process_input fname =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report ("Loading: " ^ fname) in
  let _ = parse_file fname in
  let _ = report "Original expressions: " in
  let es = exprs () in
  let _ = map (fun e -> Expr.print_expr_std e; nl ()) es in
  let _ = report "Simplified expressions: " in
  let es' = map Maxima.simplify es in
  let _ = map (fun e -> Expr.print_expr_std e; nl ()) es' in
  let _ = map gradient es in
  let _ = nl() in
  ()

let main () =
  let p = print_string in
  if Config.input_files = [] then
    let _ = p ("Usage: "
	       ^ Sys.argv.(0) 
	       ^ " [-c config1] [-c config2] ... input_file1 [input_file2 ...]\n") in
    exit 1
  else
    let _ = Config.print_options Format.std_formatter in
    let _ = map process_input Config.input_files in
    exit 0

let _ = main ()
