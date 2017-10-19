(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Symbolic Taylor forms                                                      *)
(* -------------------------------------------------------------------------- *)

(* Describes an error variable *)
type error_info = {
  (* Unique index for proofs *)
  proof_index : int;
  (* Error variables with the same index are the same *)
  index : int;
  (* The upper bound of the error is 2^exp *)
  exp : int;
}

type taylor_form = {
  form_index : int;
  v0 : Expr.expr;
  v1 : (Expr.expr * error_info) list;
}

val dummy_tform : taylor_form

val make_stronger : float -> float

val make_stronger_i : Interval.interval -> Interval.interval

val sum_high : (float * int) list -> float * int

val expr_for_index : int -> Expr.expr

val simplify_form : Expr.constraints -> taylor_form -> taylor_form

val build_form : Expr.constraints -> Expr.expr -> taylor_form
  