open List
open Rounding
open Expr
open Environment
open Taylor_form
open Log


let var_bound_float name = variable_interval name


let print_form f =
  let _ = report (Format.sprintf "v0 = %s" (print_expr_str f.v0)) in
  let _ = map (fun (e, err) -> 
    report (Format.sprintf "%d, %d: %s" err.index err.exp (print_expr_str e))) f.v1 in
  let _ = report "\nCorresponding original subexpressions:" in
  let _ = map (fun (_, err) ->
    let i = err.index in
    if i > 0 then
      let expr = expr_for_index i in
      report (Format.sprintf "%d: %s" i (print_expr_str expr))
    else ()) f.v1 in
  ()


let process_expr e =
  let _ = report "****************************" in
  let _ = report (Printf.sprintf "Taylor form for: %s" (print_expr_str e)) in
  let form = build_form var_bound_float e in
  let _ = print_form form in
  let _ = report "***************************" in
  let _ = report "Simplified form:" in
  let form = simplify_form var_bound_float form in
  let _ = print_form form in
  let _ = report "----------------------------" in
  ()


let process fname =
  let fmt = Format.std_formatter in
  let p str = Format.pp_print_string fmt str; Format.pp_print_newline fmt () in
  let pe e = p (print_expr_str e) in
  let _ = p (Printf.sprintf "Loading: %s" fname) in
  let _ = Parser.parse_file fname in
  let es = env.expressions in
  let _ = map pe es in
  let _ = p "\nSimplified:" in
  let es' = map Rounding_simpl.simplify_rounding es in
  let _ = map pe es' in
  let _ = p "\n****************************" in
  let _ = map process_expr es' in
  ()

let _ = map process Config.input_files
