(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Racket output for FPTaylor expressions                                     *)
(* -------------------------------------------------------------------------- *)

open Expr

module Out = ExprOut.Make(ExprOut.RacketIntervalPrinter)

let remove_rnd expr =
  let rec remove expr =
    match expr with
    | Const c -> expr
    | Var v -> expr
    | U_op (op, arg) -> U_op (op, remove arg)
    | Bin_op (op, arg1, arg2) -> Bin_op (op, remove arg1, remove arg2)
    | Gen_op (op, args) -> Gen_op (op, List.map remove args)
    | Rounding (rnd, arg) -> remove arg in
  remove expr

let gen_racket_function fmt (task, total2, exp, e, opt_bound) =
  let n2s = Num.string_of_num in
  let f2s f = n2s (More_num.num_of_float f) in
  let p' = Format.pp_print_string fmt in
  let var_names, var_bounds =
    let e_vars = vars_in_expr e in
    let names = Task.all_variables task in
    let vars = List.filter (fun v -> List.mem v e_vars) names in
    match vars with
    | [] -> ["unused"],
            if names <> [] then 
              [Task.variable_num_interval task (List.hd names)]
            else
              [(Num.Int 1, Num.Int 2)]
    | _ -> vars, List.map (Task.variable_num_interval task) vars in
  let bound_strings =
    List.map (fun (low, high) -> 
      Format.sprintf "(cons %s %s)" (n2s low) (n2s high))
      var_bounds in
  let vars = List.map (fun v -> v ^ "-var") var_names in
  Format.fprintf fmt "(define name \"%s\")@." task.Task.name;
  Format.fprintf fmt "(define expression-string \"%a\")@."
    (ExprOut.Info.print_fmt ~margin:max_int) (remove_rnd task.Task.expression);
  Format.fprintf fmt "(define opt-max %s)@."
    (match opt_bound with None -> "#f" | Some f -> f2s f);
  Format.fprintf fmt "(define bounds (list %s))@." (String.concat " " bound_strings);
  Format.fprintf fmt "(define eps (make-interval (bfexp2 (bf %d))))@." exp;
  Format.fprintf fmt "(define extra-errors (list (cons \"total2\" (make-interval %s))))@." (f2s total2);
  p' "(define (fptaylor-model ";
  Lib.print_list p' (fun () -> p' " ") vars;
  p' ")\n\t";
  Out.print_fmt ~margin:80 fmt e;
  p' ")"

let create_racket_file fmt task total2 exp expr opt_bound =
  gen_racket_function fmt (task, total2, exp, expr, opt_bound)
