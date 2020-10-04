open Interval
open Expr
open Opt_common

type expr' =
  | Const' of interval
  | Var' of int
  | Pown of expr' * int
  | U_op' of u_op_type * expr'
  | Bin_op' of bin_op_type * expr' * expr'
  | Gen_op' of gen_op_type * expr' list

let expr'_of_expr var_index =
  let rec of_expr = function
  | Const c -> Const' (Const.to_interval c)
  | Var v -> Var' (var_index v)
  | Rounding _ -> failwith "Rounding is not supported"
  | U_op (op, arg) -> U_op' (op, of_expr arg)
  | Bin_op (Op_mul, arg1, arg2) when eq_expr arg1 arg2 -> Pown (of_expr arg1, 2)
  | Bin_op (Op_nat_pow, arg1, arg2) ->
    let e = Eval.eval_interval_const_expr arg2 in
    let n = truncate e.low in
    if n < 0 || e.low <> e.high || float n <> e.low then
      failwith "expr'_of_expr: Op_nat_pow"
    else
      Pown (of_expr arg1, n)
  | Bin_op (op, arg1, arg2) -> Bin_op' (op, of_expr arg1, of_expr arg2)
  | Gen_op (op, args) -> Gen_op' (op, List.map of_expr args)
  in
  of_expr

let rec eval_expr' arr = function
| Const' c -> c
| Var' v -> arr.(v)
| Pown (arg, n) -> pow_I_i (eval_expr' arr arg) n
| U_op' (op, arg) ->
  begin
    let x = eval_expr' arr arg in
    match op with
    | Op_neg -> ~-$ x
    | Op_abs -> abs_I x
    | Op_inv -> inv_I x
    | Op_sqrt -> sqrt_I x
    | Op_sin -> sin_I x
    | Op_cos -> cos_I x
    | Op_tan -> tan_I x
    | Op_asin -> asin_I x
    | Op_acos -> acos_I x
    | Op_atan -> atan_I x
    | Op_exp -> exp_I x
    | Op_log -> log_I x
    | Op_sinh -> sinh_I x
    | Op_cosh -> cosh_I x
    | Op_tanh -> tanh_I x
    | Op_asinh -> Func.asinh_I x
    | Op_acosh -> Func.acosh_I x
    | Op_atanh -> Func.atanh_I x
    | Op_floor_power2 -> Func.floor_power2_I x
  end
| Bin_op' (op, arg1, arg2) ->
  begin
    let x1 = eval_expr' arr arg1 in
    let x2 = eval_expr' arr arg2 in
    match op with
    | Op_add -> x1 +$ x2
    | Op_sub -> x1 -$ x2
    | Op_mul -> x1 *$ x2
    | Op_div -> x1 /$ x2
    | Op_max -> max_I_I x1 x2
    | Op_min -> min_I_I x1 x2
    | Op_nat_pow -> x1 **$. x2.high
    | Op_sub2 -> Func.sub2_I (x1, x2)
    | Op_abs_err -> Func.abs_err_I (x1, x2)
  end
| Gen_op' (op, args) ->
  begin
    let xs = List.map (eval_expr' arr) args in
    match (op, xs) with
    | (Op_fma, [a;b;c]) -> (a *$ b) +$ c
    | _ -> failwith ("eval_expr': Unsupported general operation: " 
                     ^ gen_op_name op)
  end

let min_max_expr (pars : Opt_common.opt_pars) max_only (cs : constraints) e =
  if Config.debug () then
    Log.report `Debug "bb-eval_opt: x_abs_tol = %e, f_rel_tol = %e, f_abs_tol = %e, iters = %d"
         pars.x_abs_tol pars.f_rel_tol pars.f_abs_tol pars.max_iters;
  let var_names = vars_in_expr e in
  let start_interval = var_names
    |> List.map cs.var_interval
    |> Array.of_list in
  let x_tol = size_max_X start_interval *. pars.x_rel_tol +. pars.x_abs_tol in
  let h_vars = Hashtbl.create 8 in
  var_names |> List.iteri (fun i v -> Hashtbl.add h_vars v i);
  let e' = expr'_of_expr (Hashtbl.find h_vars) e in
  let f arr = eval_expr' arr e' in
  let fmax, lower_max, iter_max = 
    Opt0.opt f start_interval x_tol pars.f_rel_tol pars.f_abs_tol pars.max_iters in
  let fmin, lower_min, iter_min =
    if max_only then 0., 0., 0
    else
      let f_min arr = ~-$ (eval_expr' arr e') in
      let fm, lm, i = Opt0.opt f_min start_interval x_tol pars.f_rel_tol pars.f_abs_tol pars.max_iters in
      -.fm, -.lm, i in
  let rmin = {
    result = fmin;
    lower_bound = lower_min;
    iters = iter_min;
    time = 0.;
  } in
  let rmax = {
    result = fmax;
    lower_bound = lower_max;
    iters = iter_max;
    time = 0.;
  } in
  rmin, rmax