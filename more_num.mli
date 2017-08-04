(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Functions for rational and floating-point numbers                          *)
(* -------------------------------------------------------------------------- *)

val numerator : Num.num -> Big_int.big_int
val denominator : Num.num -> Big_int.big_int

val num_of_float_string : string -> Num.num
val num_of_float : float -> Num.num

val is_power_of_two : Num.num -> bool

val is_nan : float -> bool
val is_infinity : float -> bool
val next_float : float -> float
val prev_float : float -> float

val log2_num : Num.num -> int

val float_of_num_lo : Num.num -> float
val float_of_num_hi : Num.num -> float

val interval_of_num : Num.num -> Interval.interval
val interval_of_string : string -> Interval.interval

val check_interval : Interval.interval -> string