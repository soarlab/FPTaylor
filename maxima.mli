(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Interface to Maxima                                                        *)
(* -------------------------------------------------------------------------- *)

val maxima : string -> string

val test_maxima : unit -> bool
   
val simplify : Task.task -> Expr.expr -> Expr.expr

val diff : Task.task -> string -> Expr.expr -> Expr.expr

val simplify_diff : Task.task -> string -> Expr.expr -> Expr.expr

val taylor_coeff : string -> int -> Expr.expr -> string

val taylor_coeff1 : string list -> Expr.expr -> string list

