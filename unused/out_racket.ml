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

let gen_racket_function fmt (task, extra_errors, opt_bound, exprs) =
  let n2s = Num.string_of_num in
  let f2s f = n2s (More_num.num_of_float f) in
  let p' = Format.pp_print_string fmt in
  let var_names, var_bounds =
    let e_vars = Lib.unions (List.map vars_in_expr exprs) in
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
  if List.length extra_errors > 0 then
    let extra_strs =
      List.map
        (fun (name, err) -> Format.sprintf "(cons \"%s\" (make-interval %s))" name (f2s err))
        extra_errors in
    Format.fprintf fmt "(define extra-errors (list %s))@." (String.concat " " extra_strs)
  else
    Format.fprintf fmt "(define extra-errors '())@.";
  if List.length exprs = 0 then
    Format.fprintf fmt "(define functions '())@."
  else begin
    Format.fprintf fmt "(define functions (list@.";
    List.iter 
      (fun e -> 
        p' "  (lambda (";
        Lib.print_list p' (fun () -> p' " ") vars;
        p' ")\n    ";
        Out.print_fmt ~margin:80 fmt e;
        p' ")") exprs;
    Format.fprintf fmt "@.  ))@."
  end

let create_racket_file fmt task ?(extra_errors = []) ?opt_bound exprs =
  gen_racket_function fmt (task, extra_errors, opt_bound, exprs)
