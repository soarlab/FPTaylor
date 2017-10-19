(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with a simple branch and bound procedure                      *) 
(* (see b_and_b/opt0.ml)                                                      *)
(* -------------------------------------------------------------------------- *)

open Opt_common

val min_max_expr : opt_pars -> bool -> Expr.constraints -> Expr.expr -> opt_result * opt_result
