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

   This interval library needs the OCaml [Num] module.

   It is assumed that all floating-point operations are IEEE 754
   compatible and the rounding mode is to nearest.

   It is also assumed that OCaml functions [exp], [log] compute results with
   less than 1 ulp error. 

   Intervals computed with this library are optimal floating-point
   intervals for basic arithmetic operations. 

   {!Interval1} provides faster interval functions which are only
   slightly less optimal.
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
                  
(** Computes a midpoint of an interval. This function returns finite
   values for all valid non-empty intervals. *)
val mid_i : interval -> float

(** Interval negation {b (optimal)} *)
val neg_i : interval -> interval

(** Interval absolute value {b (optimal)} *)
val abs_i : interval -> interval

(** Interval maximum {b (optimal)} *)
val max_ii : interval -> interval -> interval

(** Interval minimum {b (optimal)} *)
val min_ii : interval -> interval -> interval

(** Interval addition {b (optimal)} *)
val add_ii : interval -> interval -> interval

(** Addition of an interval and a number {b (optimal)} *)
val add_id : interval -> float -> interval

(** Addition of a number and an interval {b (optimal)} *)
val add_di : float -> interval -> interval

(** Interval subtraction {b (optimal)} *)
val sub_ii : interval -> interval -> interval

(** Subtraction of an interval and a number {b (optimal)} *)
val sub_id : interval -> float -> interval

(** Subtraction of a number and an interval {b (optimal)} *)
val sub_di : float -> interval -> interval

(** Interval multiplication {b (optimal)} *)
val mul_ii : interval -> interval -> interval

(** Multiplication of an interval and a number {b (optimal)} *)
val mul_id : interval -> float -> interval

(** Multiplication of a number and an interval {b (optimal)} *)
val mul_di : float -> interval -> interval

(** Interval division {b (optimal)} *)
val div_ii : interval -> interval -> interval

(** Division of an interval by a number {b (optimal)} *)
val div_id : interval -> float -> interval

(** Division of a number by an interval {b (optimal)} *)
val div_di : float -> interval -> interval

(** Interval reciprocal {b (optimal)} *)
val inv_i : interval -> interval

(** Interval square root {b (optimal)} *)         
val sqrt_i : interval -> interval

(** Interval square {b (optimal)} *)
val sqr_i : interval -> interval

(** Interval integer power. This function returns an optimal interval
    but this behavior may change in the future. *)
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

(** Computes a successor of a floating-point number {b (optimal)} *)
val fsucc : float -> float

(** Computes a predecessor of a floating-point number {b (optimal)} *)
val fpred : float -> float

(** Returns a lower bound of the sum of two floating-point numbers {b
   (optimal)} *)
val fadd_low : float -> float -> float

(** Returns an upper bound of the sum of two floating-point numbers {b
   (optimal)} *)
val fadd_high : float -> float -> float

(** Returns a lower bound of the difference of two floating-point
   numbers {b (optimal)} *)
val fsub_low : float -> float -> float

(** Returns an upper bound of the difference of two floating-point
   numbers {b (optimal)} *)
val fsub_high : float -> float -> float

(** Returns a lower bound of the product of two floating-point numbers
   {b (optimal)} *)
val fmul_low : float -> float -> float

(** Returns an upper bound of the product of two floating-point
   numbers {b (optimal)} *)
val fmul_high : float -> float -> float
                                    
(** Returns a lower bound of the ratio of two floating-point numbers
   {b (optimal)} *)
val fdiv_low : float -> float -> float

(** Returns an upper bound of the ratio of two floating-point numbers
   {b (optimal)} *)
val fdiv_high : float -> float -> float

(** Returns a lower bound of [x^2] {b (optimal)} *)
val fsqr_low : float -> float

(** Returns an upper bound of [x^2] {b (optimal)} *)
val fsqr_high : float -> float
                                    
(** Returns a lower bound of [sqrt x] {b (optimal)} *)
val fsqrt_low : float -> float

(** Returns an upper bound of [sqrt x] {b (optimal)} *)
val fsqrt_high : float -> float

(** Returns a lower bound of [exp x] *)
val fexp_low : float -> float

(** Returns an upper bound of [exp x] *)
val fexp_high : float -> float

(** Returns a lower bound of [log x] *)
val flog_low : float -> float

(** Returns an upper bound of [log x] *)
val flog_high : float -> float

(*
(** Return a lower bound of [cos x] *)
val fcos_low : float -> float

(** Returns an upper bound of [cos x] *)
val fcos_high : float -> float

(** Returns a lower bound of [sin x] *)
val fsin_low : float -> float

(** Returns an upper bound of [sin x] *)
val fsin_high : float -> float
*)
                           
(** Returns a lower bound of [x^n] *)
val fpown_low : float -> int -> float

(** Returns an upper bound of [x^n] *)
val fpown_high : float -> int -> float
