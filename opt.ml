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

let optimize (pars : Opt_common.opt_pars) e =
  let min, max =
    match Config.get_string_option "opt" with
    | "z3" -> 
       Opt_z3.min_max_expr pars var_bound_rat e
    | "bb" -> 
       Opt_basic_bb.min_max_expr pars var_bound_float e
    | "nlopt" -> 
       Opt_nlopt.min_max_expr pars var_bound_float e
    | "gelpia" -> 
       Opt_gelpia.min_max_expr pars var_bound_float e
    | s -> failwith ("Unsupported optimization backend: " ^ s) in
  min, max

let optimize_abs pars e =
  let min_f, max_f = optimize pars e in
  max (abs_float min_f) (abs_float max_f)

