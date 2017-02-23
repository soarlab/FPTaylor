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

type t = Rat of num | Interval of interval

let of_num n = Rat n

let of_int i = Rat (Int i)

let of_float f = Rat (More_num.num_of_float f)
                                         
let of_interval v =
  if v.low = v.high then of_float v.low
  else if v.low < v.high && neg_infinity < v.high && v.low < infinity then Interval v
  else failwith ("Const.of_interval: bad argument: " ^ sprintf_I "%.20e" v)

let is_rat_const = function
  | Rat _ -> true
  | Interval _ -> false

let is_interval_const = function
  | Rat _ -> false
  | Interval _ -> true
                
let to_interval = function
  | Interval v -> v
  | Rat n -> More_num.interval_of_num n

let to_float = function
  | Interval v -> 0.5 *. (v.low +. v.high)
  | Rat n -> Num.float_of_num n

let to_num = function
  | Rat n -> n
  | Interval _ -> failwith "Const.to_num: interval constant"

let low_bound_to_num = function
  | Rat n -> n
  | Interval v -> More_num.num_of_float v.low

let high_bound_to_num = function
  | Rat n -> n
  | Interval v -> More_num.num_of_float v.high
                                      
let lift_u_ops (op_n, op_i) c =
  match c with
  | Rat n -> of_num (op_n n)
  | Interval v -> of_interval (op_i v)

let lift_bin_ops (op_n, op_i) c1 c2 =
  match (c1, c2) with
  | Rat n1, Rat n2 -> of_num (op_n n1 n2)
  | Interval _, _ | _, Interval _ ->
     of_interval (op_i (to_interval c1) (to_interval c2))

let neg_c = lift_u_ops (minus_num, (~-$))

let abs_c = lift_u_ops (abs_num, abs_I)

let min_c = lift_bin_ops (min_num, min_I_I)

let max_c = lift_bin_ops (max_num, max_I_I)

let add_c = lift_bin_ops (add_num, (+$))

let sub_c = lift_bin_ops (sub_num, (-$))

let mul_c = lift_bin_ops (mult_num, ( *$ ))

let div_c = lift_bin_ops (div_num, (/$))

let eq_c c1 c2 =
  match (c1, c2) with
  | Rat n1, Rat n2 -> n1 =/ n2
  | Interval v1, Interval v2 -> v1.low = v2.low && v1.high = v2.high
  | _ -> false
