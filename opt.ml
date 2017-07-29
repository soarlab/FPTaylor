(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* General optimization functions                                             *)
(* -------------------------------------------------------------------------- *)

open Expr
open Environment
open Opt_common

let var_bound_float name = 
  variable_interval name

let var_bound_rat name =
  let v = find_variable name in
  Const.low_bound_to_num v.lo_bound, Const.high_bound_to_num v.hi_bound

let optimize_expr (pars : Opt_common.opt_pars) max_only expr =
  let rmin, rmax =
    match Config.get_string_option "opt" with
    | "z3" -> 
      let bounds = 
        try Eval.eval_interval_expr var_bound_float expr
        with _ -> {Interval.low = neg_infinity; Interval.high = infinity} in
      Log.report `Debug "Interval bounds for Z3: %s" (Interval.sprintf_I "%.5e" bounds);
      let fmin, fmax = Opt_z3.min_max_expr pars max_only var_bound_rat bounds expr in
      {empty_result with result = fmin; lower_bound = infinity},
      {empty_result with result = fmax; lower_bound = neg_infinity}
    | "bb" -> 
      Opt_basic_bb.min_max_expr pars max_only var_bound_float expr
    | "nlopt" -> 
      let fmin, fmax = Opt_nlopt.min_max_expr pars var_bound_float expr in
      {empty_result with result = fmin; lower_bound = infinity},
      {empty_result with result = fmax; lower_bound = neg_infinity}
    | "gelpia" -> 
      let fmin, fmax = Opt_gelpia.min_max_expr pars max_only var_bound_float expr in
      {empty_result with result = fmin; lower_bound = infinity},
      {empty_result with result = fmax; lower_bound = neg_infinity}
    | s -> failwith ("Unsupported optimization backend: " ^ s) in
  rmin, rmax

let find_min_max pars expr =
  let rmin, rmax = optimize_expr pars false expr in
  rmin.result, rmax.result

let find_max pars expr =
  let _, rmax = optimize_expr pars true expr in
  rmax

let find_max_abs pars expr =
  let rmin, rmax = optimize_expr pars false expr in
  let r =
    if abs_float rmax.result >= abs_float rmin.result then rmax else rmin in
  if r.result >= 0. then r
  else
    {r with result = abs_float r.result; lower_bound = -.r.lower_bound}

