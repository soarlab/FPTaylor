(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* General optimization functions                                             *)
(* -------------------------------------------------------------------------- *)

open Expr
open Environment

let var_bound_float name = 
  variable_interval name

let var_bound_rat name =
  let v = find_variable name in
  v.lo_bound.rational_v, v.hi_bound.rational_v

let optimize_expr (pars : Opt_common.opt_pars) max_only expr =
  let min, max =
    match Config.get_string_option "opt" with
    | "z3" -> 
       Opt_z3.min_max_expr pars var_bound_rat expr
    | "bb" -> 
       Opt_basic_bb.min_max_expr pars max_only var_bound_float expr
    | "nlopt" -> 
       Opt_nlopt.min_max_expr pars var_bound_float expr
    | "gelpia" -> 
       Opt_gelpia.min_max_expr pars var_bound_float expr
    | s -> failwith ("Unsupported optimization backend: " ^ s) in
  min, max

let find_min_max pars expr =
  optimize_expr pars false expr

let find_max pars expr =
  let _, max = optimize_expr pars true expr in
  max

let find_max_abs pars expr =
  let min_f, max_f = optimize_expr pars false expr in
  max (abs_float min_f) (abs_float max_f)

