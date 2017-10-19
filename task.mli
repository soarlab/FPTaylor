(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* FPTaylor input task                                                        *)
(* -------------------------------------------------------------------------- *)

type var_info = {
  var_name : string;
  var_type : Rounding.value_type;
  lo_bound : Const.t;
  hi_bound : Const.t;
  uncertainty : Const.t;
}

type task = {
  name : string;
  expression : Expr.expr;
  variables : var_info list;
  constraints : (string * Expr.formula) list;
}

val find_variable : task -> string -> var_info

val variable_type : task -> string -> Rounding.value_type

val variable_interval : task -> string -> Interval.interval

val variable_num_interval : task -> string -> Num.num * Num.num

val constraints_of_task : task -> Expr.constraints
