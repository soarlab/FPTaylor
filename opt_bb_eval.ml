open Interval
open Expr
open Opt_common

let t = Opt0.opt

let min_max_expr (pars : Opt_common.opt_pars) max_only (cs : constraints) e =
  if Config.debug then
    Log.report `Debug "bb-eval_opt: x_abs_tol = %e, f_rel_tol = %e, f_abs_tol = %e, iters = %d"
         pars.x_abs_tol pars.f_rel_tol pars.f_abs_tol pars.max_iters;
  let var_names = vars_in_expr e in
  let start_interval = var_names
    |> List.map cs.var_interval
    |> Array.of_list in
  let x_tol = size_max_X start_interval *. pars.x_rel_tol +. pars.x_abs_tol in
  let h_vars = Hashtbl.create 8 in
  let vars arr name = arr.(Hashtbl.find h_vars name) in
  let f arr = Eval.eval_interval_expr (vars arr) e in
  var_names |> List.iteri (fun i v -> Hashtbl.add h_vars v i);
  let upper_bound, lower_bound, c = 
    Opt0.opt f start_interval x_tol pars.f_rel_tol pars.f_abs_tol pars.max_iters in
  let rmin = empty_result in
  let rmax = {
    result = upper_bound;
    lower_bound = lower_bound;
    iters = c;
    time = 0.;
  } in
  rmin, rmax