(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with the nlopt library                                        *)
(* -------------------------------------------------------------------------- *)

open Opt_common

val min_max_expr : opt_pars -> Expr.constraints -> Expr.expr -> float * float
