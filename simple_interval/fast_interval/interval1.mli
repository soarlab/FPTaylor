(* ========================================================================== *)
(*      A simple OCaml interval library                                       *)
(*      https://github.com/monadius/ocaml_simple_interval                     *)
(*                                                                            *)
(*      Author: Alexey Solovyev                                               *)
(*      https://github.com/monadius                                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(** A simple OCaml interval library.

   This interval library does not depend on any external files and libraries.

   It is assumed that all floating-point operations are IEEE 754
   compatible and the rounding mode is to nearest.

   It is also assumed that OCaml functions [exp], [log] compute results with
   less than 1 ulp error. 

   Intervals computed with this library may be not the optimal
   floating-point intervals. But the error for each endpoint is at
   most 1 ulp (2 ulp for some exceptional cases near the subnormal
   range) for most functions (the error of [pown_i] can be larger).
*)

(** The interval type *)
type interval = {
    low : float;
    high : float;
  }

(** The empty interval *)
val empty_interval : interval

(** The entire interval representing (-infinity, infinity) *)
val entire_interval : interval

(** [[0., 0.]] *)
val zero_interval : interval

(** [[1., 1.]] *)
val one_interval : interval

(** {6 Interval operations} *)
                     
(** Creates an interval from given endpoints *)
val make_interval : float -> float -> interval

(** Tests if an interval is empty *)                                        
val is_empty : interval -> bool

(** Tests if an interval is the entire interval *)
val is_entire : interval -> bool

(** Tests if an interval is valid. A valid interval is either empty
   or [[a, b]] with [a <= b], [a < infinity], [-infinity < b]. *)
val is_valid : interval -> bool
                  
(** Computes a midpoint of an interval as [(a + b) / 2].  This function
   may return incorrect results when [a + b] overflows or for the entire
   interval. *)
val mid_i_fast : interval -> float

(** Computes a midpoint of an interval. This function returns finite
   values for all valid non-empty intervals. *)
val mid_i : interval -> float

(** Interval negation *)
val neg_i : interval -> interval

(** Interval absolute value *)
val abs_i : interval -> interval

(** Interval maximum *)
val max_ii : interval -> interval -> interval

(** Interval minimum *)
val min_ii : interval -> interval -> interval
                           
(** Interval addition *)
val add_ii : interval -> interval -> interval

(** Addition of an interval and a number *)
val add_id : interval -> float -> interval

(** Addition of a number and an interval *)
val add_di : float -> interval -> interval

(** Interval subtraction *)
val sub_ii : interval -> interval -> interval

(** Subtraction of an interval and a number *)
val sub_id : interval -> float -> interval

(** Subtraction of a number and an interval *)
val sub_di : float -> interval -> interval

(** Interval multiplication *)
val mul_ii : interval -> interval -> interval

(** Multiplication of an interval and a number *)
val mul_id : interval -> float -> interval

(** Multiplication of a number and an interval *)
val mul_di : float -> interval -> interval

(** Interval division *)
val div_ii : interval -> interval -> interval

(** Division of an interval by a number *)
val div_id : interval -> float -> interval

(** Division of a number by an interval *)
val div_di : float -> interval -> interval

(** Interval reciprocal *)
val inv_i : interval -> interval

(** Interval square root *)         
val sqrt_i : interval -> interval

(** Interval square *)
val sqr_i : interval -> interval

(** Interval integer power *)
val pown_i : interval -> int -> interval

(** Interval exponential function. It is assumed that the standard
   function [exp:float->float] has less than 1 ulp error. *)
val exp_i : interval -> interval

(** Interval natural logarithm. It is assumed that the standard
   function [log:float->float] has less than 1 ulp error. *)
val log_i : interval -> interval

(** Interval sine (not implemented yet) *)
val sin_i : interval -> interval

(** Interval cosine (not implemented yet) *)
val cos_i : interval -> interval

(** {6 Floating-point operations with directed rounding} *)

(** Computes a successor of a floating-point number *)
val fsucc : float -> float

(** Computes a predecessor of a floating-point number *)
val fpred : float -> float

(** Returns a lower bound of the sum of two floating-point numbers *)
val fadd_low : float -> float -> float

(** Returns an upper bound of the sum of two floating-point numbers *)
val fadd_high : float -> float -> float

(** Returns a lower bound of the difference of two floating-point numbers *)
val fsub_low : float -> float -> float

(** Returns an upper bound of the difference of two floating-point numbers *)
val fsub_high : float -> float -> float

(** Returns a lower bound of the product of two floating-point numbers *)
val fmul_low : float -> float -> float

(** Returns an upper bound of the product of two floating-point numbers *)
val fmul_high : float -> float -> float
                                    
(** Returns a lower bound of the ratio of two floating-point numbers *)
val fdiv_low : float -> float -> float

(** Returns an upper bound of the ratio of two floating-point numbers *)
val fdiv_high : float -> float -> float

(** Returns a lower bound of [x^2] *)
val fsqr_low : float -> float

(** Returns an upper bound of [x^2] *)
val fsqr_high : float -> float

(** Returns a lower bound of [sqrt x] *)
val fsqrt_low : float -> float

(** Returns an upper bound of [sqrt x] *)
val fsqrt_high : float -> float

(** Returns a lower bound of [exp x] *)
val fexp_low : float -> float

(** Returns an upper bound of [exp x] *)
val fexp_high : float -> float

(** Returns a lower bound of [log x] *)
val flog_low : float -> float

(** Returns an upper bound of [log x] *)
val flog_high : float -> float

(** Return a lower bound of [cos x] *)
val fcos_low : float -> float

(** Returns an upper bound of [cos x] *)
val fcos_high : float -> float

(** Returns a lower bound of [sin x] *)
val fsin_low : float -> float

(** Returns an upper bound of [sin x] *)
val fsin_high : float -> float

(** Returns a lower bound of [x^n] *)
val fpown_low : float -> int -> float

(** Returns an upper bound of [x^n] *)
val fpown_high : float -> int -> float
