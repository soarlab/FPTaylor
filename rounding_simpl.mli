(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Rounding simplification procedures                                         *)
(* -------------------------------------------------------------------------- *)

exception Exceptional_operation of Expr.expr * string

val get_type : (string -> Rounding.value_type) -> Expr.expr -> Rounding.value_type

val simplify_rounding : (string -> Rounding.value_type) -> Expr.expr -> Expr.expr

val check_expr : (string -> Interval.interval) -> Expr.expr -> Interval.interval
