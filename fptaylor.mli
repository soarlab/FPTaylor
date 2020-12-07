(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Main FPTaylor functions                                                    *)
(* -------------------------------------------------------------------------- *)

type error_type = 
  Err_abs_approx | Err_abs_exact | 
  Err_rel_approx | Err_rel_exact |
  Err_ulp_approx | Err_ulp_exact

type error_result = {
  error_type : error_type;
  (* Total error *)
  (* Lower bounds of error intervals represent lower bounds
     returned by a global optimization procedure.
     low = neg_infinity if a lower bound is not returned. *)
  error : Interval.interval option;
  (* Second order error *)
  total2 : Interval.interval option;
  (* Error model *)
  error_model : Expr.expr option;
}

type result = {
  task : Task.task;
  real_bounds : Interval.interval;
  errors : error_result list;
  elapsed_time : float;
}

val validate_options : unit -> unit

val fptaylor : input_files:string list -> result list