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

open Rounding
open Expr
open Interval

type var_info = {
  var_name : string;
  var_type : value_type;
  lo_bound : Const.t;
  hi_bound : Const.t;
  uncertainty : Const.t;
}

type task = {
  name : string;
  expression : expr;
  variables : var_info list;
  constraints : (string * formula) list;
}

let find_variable t name =
  List.find (fun v -> v.var_name = name) t.variables

let variable_type t name =
  (find_variable t name).var_type

let variable_interval t name =
  let var = find_variable t name in {
    low = (Const.to_interval var.lo_bound).low;
    high = (Const.to_interval var.hi_bound).high;
  }

let variable_num_interval t name =
  let var = find_variable t name in
  let low = Const.low_bound_to_num var.lo_bound in
  let high = Const.high_bound_to_num var.hi_bound in
  (low, high)

let constraints_of_task t = {
  var_interval = variable_interval t;
  var_rat_bounds = variable_num_interval t;
  constraints = List.map snd t.constraints;
}