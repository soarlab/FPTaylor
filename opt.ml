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
open Opt_common

let optimize_expr (pars : Opt_common.opt_pars) max_only (cs : constraints) expr =
  let rmin, rmax =
    match Config.get_string_option "opt" with
    | "z3" -> 
      let bounds = 
        try Eval.eval_interval_expr cs.var_interval expr
        with _ -> {Interval.low = neg_infinity; Interval.high = infinity} in
      Log.report `Debug "Interval bounds for Z3: %s" (Interval.sprintf_I "%.5e" bounds);
      let fmin, fmax = Opt_z3.min_max_expr pars max_only cs bounds expr in
      {empty_result with result = fmin; lower_bound = infinity},
      {empty_result with result = fmax; lower_bound = neg_infinity}
    | "bb" -> 
      Opt_basic_bb.min_max_expr pars max_only cs expr
    | "nlopt" -> 
      let fmin, fmax = Opt_nlopt.min_max_expr pars cs expr in
      {empty_result with result = fmin; lower_bound = infinity},
      {empty_result with result = fmax; lower_bound = neg_infinity}
    | "gelpia" -> 
      let fmin, fmax = Opt_gelpia.min_max_expr pars max_only cs expr in
      {empty_result with result = fmin; lower_bound = infinity},
      {empty_result with result = fmax; lower_bound = neg_infinity}
    | s -> failwith ("Unsupported optimization backend: " ^ s) in
  rmin, rmax

let find_min_max pars cs expr =
  let rmin, rmax = optimize_expr pars false cs expr in
  rmin.result, rmax.result

let find_max pars cs expr =
  let _, rmax = optimize_expr pars true cs expr in
  rmax

let find_max_abs pars cs expr =
  let rmin, rmax = optimize_expr pars false cs expr in
  Log.report `Debug "rmin(result = %e, lower = %e), rmax(result = %e, lower = %e)"
    rmin.result rmin.lower_bound rmax.result rmax.lower_bound;
  let r = if abs_float rmax.result >= abs_float rmin.result then rmax else rmin in
  { r with
    result = abs_float r.result;
    lower_bound =
      if More_num.is_infinity r.lower_bound then 
        neg_infinity
      else if r.result >= 0. then
        max 0. r.lower_bound
      else
        max 0. (-.r.lower_bound)
  }
