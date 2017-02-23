(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Rational and interval constants                                            *)
(* -------------------------------------------------------------------------- *)

open Num
open Interval

type t = private Rat of num | Interval of interval

val of_num : num -> t

val of_int : int -> t

val of_float : float -> t

val of_interval : interval -> t

val to_interval : t -> interval

val is_rat_const : t -> bool

val is_interval_const : t -> bool

val to_num : t -> num

val low_bound_to_num : t -> num

val high_bound_to_num : t -> num
                         
val to_float : t -> float

val neg_c : t -> t

val abs_c : t -> t

val min_c : t -> t -> t

val max_c : t -> t -> t

val add_c : t -> t -> t

val sub_c : t -> t -> t

val mul_c : t -> t -> t

val div_c : t -> t -> t

val eq_c : t -> t -> bool
