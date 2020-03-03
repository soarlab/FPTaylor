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

(*
description: Type for constants, which can be a num (build-in of Ocaml) or interval
*)
type t = Rat of num | Interval of interval

(*
input: num
output: t
description: coverts num to constant (t type)
*)
let of_num n = Rat n

(*
input: int
output: t
description: coverts int to constant (t type)
*)
let of_int i = Rat (Int i)

(*
input: float
output: t
description: cover float to constant (t type)
*)
let of_float f = Rat (More_num.num_of_float f)
                                         
(*
input: interval
output: t
description: coverts interval to constant (t type)
*)
let of_interval v =
  if v.low = v.high then of_float v.low
  else if v.low < v.high && neg_infinity < v.high && v.low < infinity then Interval v
  else failwith ("Const.of_interval: bad argument: " ^ sprintf_I "%.20e" v)

(*
input: t
output: bool
description: return true if the constrant is a num
*)
let is_rat = function
  | Rat _ -> true
  | Interval _ -> false

(*
input: interval
output: bool
description: return true if the constant is an interval
*)
let is_interval = function
  | Rat _ -> false
  | Interval _ -> true
                
(*
input: t
output: interval
description: cover a constant to an interval, if it is a num, return the interval of 2 64-bit floats that contains the num with smallest range
*)
let to_interval = function
  | Interval v -> v
  | Rat n -> More_num.interval_of_num n

(*
input: t
output: foat
description: cover a constant to float, if it is an interval, return the mean of 2 bounds
*)
let to_float = function
  | Interval v -> 0.5 *. (v.low +. v.high)
  | Rat n -> Num.float_of_num n

(*
input: t
output: num
description: cover a constant to num, if it is an interval, raise an error
*)
let to_num = function
  | Rat n -> n
  | Interval _ -> failwith "Const.to_num: interval constant"

(*
input: t
output: int
description: cover a constant to integer
*)
let to_int c = int_of_num (to_num c)

(*
input: t
output: num
description: coverts the lower bound of an interval constant to num, if the constant is num, just return it
*)
let low_bound_to_num = function
  | Rat n -> n
  | Interval v -> More_num.num_of_float v.low

(*
input: t
output: num
description: coverts the upper bound of an interval constant to num, if the constant is num, just return it
*)
let high_bound_to_num = function
  | Rat n -> n
  | Interval v -> More_num.num_of_float v.high
                                      
(*
input: (num->num,interval->interval), t
output: t
description: apply the unary function/operator over the constant and return the new constrant
*)
let lift_u_ops (op_n, op_i) c =
  match c with
  | Rat n -> of_num (op_n n)
  | Interval v -> of_interval (op_i v)

(*
input: (num->num->num,interval->interval->interval), t
output: t
description: apply the binary function/operator over the constant and return the new constrant
*)
let lift_bin_ops (op_n, op_i) c1 c2 =
  match (c1, c2) with
  | Rat n1, Rat n2 -> of_num (op_n n1 n2)
  | Interval _, _ | _, Interval _ ->
     of_interval (op_i (to_interval c1) (to_interval c2))

(*
input: t
output: t
description: return the constant as the negative of the original one
*)
let neg_c = lift_u_ops (minus_num, (~-$))

(*
input: t
output: t
description: return the constant as the absolute value of the original one
*)
let abs_c = lift_u_ops (abs_num, abs_I)

(*
input: t,t
output: t
description: return the constant as the minimum of the 2 constants
*)
let min_c = lift_bin_ops (min_num, min_I_I)

(*
input: t,t
output: t
description: return the constant as the maximum of the 2 constants
*)
let max_c = lift_bin_ops (max_num, max_I_I)

(*
input: t,t
output: t
description: return the constant as the addition of the 2 constants
*)
let add_c = lift_bin_ops (add_num, (+$))

(*
input: t,t
output: t
description: return the constant as the subtraction of the 2 constants
*)
let sub_c = lift_bin_ops (sub_num, (-$))

(*
input: t,t
output: t
description: return the constant as the multiplication of the 2 constants
*)
let mul_c = lift_bin_ops (mult_num, ( *$ ))

(*
input: t,t
output: t
description: return the constant as the division of the 2 constants
*)
let div_c = lift_bin_ops (div_num, (/$))

(*
input: t, t
output: bool
description: return true if the 2 constants are the same
*)
let eq_c c1 c2 =
  match (c1, c2) with
  | Rat n1, Rat n2 -> n1 =/ n2
  | Interval v1, Interval v2 -> v1.low = v2.low && v1.high = v2.high
  | _ -> false
