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

let optimize tolf e =
  let min, max =
    match Config.get_string_option "opt" with
      | "z3" -> 
	Opt_z3.min_max_expr tolf var_bound_rat e
      | "bb" -> 
	let tolx = Config.get_float_option "bb-tolx" in
	let max_iter = Config.get_int_option "bb-iter" in
	Opt_basic_bb.min_max_expr tolx tolf max_iter var_bound_float e
      | "nlopt" -> 
	Opt_nlopt.min_max_expr tolf var_bound_float e
      | "gelpia" -> 
	let tolx = Config.get_float_option "gelpia-tolx" in
	Opt_gelpia.abs_max_expr tolx tolf var_bound_float e
      | s -> failwith ("Unsupported optimization backend: " ^ s) in
  min, max

let optimize_abs tolf e =
  let min_f, max_f = optimize tolf e in
  max (abs_float min_f) (abs_float max_f)

