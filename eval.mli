(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Numerical (float, rational, interval) evaluation of symbolic expressions   *)
(* -------------------------------------------------------------------------- *)

val eval_float_expr : (string -> float) -> Expr.expr -> float

val eval_float_const_expr : Expr.expr -> float

val eval_num_expr : (string -> Num.num) -> Expr.expr -> Num.num

val eval_num_const_expr : Expr.expr -> Num.num

val eval_interval_expr : ?cache:Interval.interval Expr.ExprHashtbl.t -> (string -> Interval.interval) -> 
                          Expr.expr -> Interval.interval

val eval_interval_const_expr : Expr.expr -> Interval.interval

val eval_const_expr : Expr.expr -> Const.t
