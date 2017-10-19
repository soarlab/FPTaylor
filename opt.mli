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

open Opt_common

val optimize_expr : opt_pars -> bool -> Expr.constraints -> Expr.expr -> opt_result * opt_result

val find_min_max : opt_pars -> Expr.constraints -> Expr.expr -> float * float

val find_max : opt_pars -> Expr.constraints -> Expr.expr -> opt_result

val find_max_abs : opt_pars -> Expr.constraints -> Expr.expr -> opt_result