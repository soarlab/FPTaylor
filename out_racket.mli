(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Racket output for FPTaylor expressions                                     *)
(* -------------------------------------------------------------------------- *)

val create_racket_file : Format.formatter -> Task.task -> float -> int -> Expr.expr -> float option -> unit
