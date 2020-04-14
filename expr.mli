(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Symbolic expressions                                                       *)
(* -------------------------------------------------------------------------- *)

type u_op_type = 
  | Op_neg
  | Op_abs
  | Op_inv
  | Op_sqrt
  | Op_sin
  | Op_zero_sin
  | Op_one_sin
  | Op_m_one_sin
  | Op_taylor_1_sin
  | Op_taylor_3_sin
  | Op_cos
  | Op_tan
  | Op_asin
  | Op_acos
  | Op_atan
  | Op_exp
  | Op_log
  | Op_sinh
  | Op_cosh
  | Op_tanh
  | Op_asinh
  | Op_acosh
  | Op_atanh
  | Op_floor_power2

type bin_op_type =
  | Op_max
  | Op_min
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_nat_pow
  | Op_sub2
  | Op_abs_err

type gen_op_type =
  | Op_fma
  | Op_ulp

type expr =
  | Const of Const.t
  | Var of string
  | Rounding of Rounding.rnd_info * expr
  | U_op of u_op_type * expr
  | Bin_op of bin_op_type * expr * expr
  | Gen_op of gen_op_type * expr list

type formula =
  | Le of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr

type constraints = {
  var_interval : string -> Interval.interval;
  var_rat_bounds : string -> Num.num * Num.num;
  var_uncertainty : string -> Const.t;
  constraints : formula list;
}

val mk_const : Const.t -> expr
val mk_var : string -> expr
val mk_rounding : Rounding.rnd_info -> expr -> expr
                                         
val mk_neg : expr -> expr
val mk_abs : expr -> expr
val mk_sqrt : expr -> expr
val mk_inv : expr -> expr
val mk_sin : expr -> expr
val mk_zero_sin : expr -> expr
val mk_one_sin : expr -> expr
val mk_m_one_sin : expr -> expr
val mk_taylor_1_sin : expr -> expr
val mk_taylor_3_sin : expr -> expr
val mk_cos : expr -> expr
val mk_tan : expr -> expr
val mk_asin : expr -> expr
val mk_acos : expr -> expr
val mk_atan : expr -> expr
val mk_exp : expr -> expr
val mk_log : expr -> expr
val mk_sinh : expr -> expr
val mk_cosh : expr -> expr
val mk_tanh : expr -> expr
val mk_asinh : expr -> expr
val mk_acosh : expr -> expr
val mk_atanh : expr -> expr

val mk_max : expr -> expr -> expr
val mk_min : expr -> expr -> expr
val mk_add : expr -> expr -> expr
val mk_sub : expr -> expr -> expr
val mk_mul : expr -> expr -> expr
val mk_div : expr -> expr -> expr
val mk_nat_pow : expr -> expr -> expr
val mk_fma : expr -> expr -> expr -> expr
                                       
val mk_sub2 : expr -> expr -> expr
val mk_abs_err : expr -> expr -> expr
val mk_floor_power2 : expr -> expr
val mk_floor_sub2 : expr -> expr -> expr

val mk_ulp : int * int -> expr -> expr
                                        
val mk_int_const : int -> expr
val mk_num_const : Num.num -> expr
val mk_float_const : float -> expr
val mk_interval_const : Interval.interval -> expr
                                   
val const_0 : expr
val const_1 : expr
val const_2 : expr
val const_3 : expr
val const_4 : expr
val const_5 : expr

val u_op_name : u_op_type -> string
val bin_op_name : bin_op_type -> string
val gen_op_name : gen_op_type -> string

val eq_expr : expr -> expr -> bool

val vars_in_expr : expr -> string list
