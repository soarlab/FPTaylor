(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with the Z3 SMT solver (see z3opt.py)                         *)
(* -------------------------------------------------------------------------- *)

open Opt_common

val min_max_expr : opt_pars -> bool -> Expr.constraints -> Interval.interval -> Expr.expr -> float * float
