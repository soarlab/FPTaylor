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

type problem = {
  name : string;
  expression : expr;
  variables : var_info list;
  constraints : (string * formula) list;
}

let find_variable p name =
  List.find (fun v -> v.var_name = name) p.variables

let variable_type p name =
  (find_variable p name).var_type

let variable_interval p name =
  let var = find_variable p name in {
    low = (Const.to_interval var.lo_bound).low;
    high = (Const.to_interval var.hi_bound).high;
  }

let variable_num_interval p name =
  let var = find_variable p name in
  let low = Const.low_bound_to_num var.lo_bound in
  let high = Const.high_bound_to_num var.hi_bound in
  (low, high)

let constraints_of_problem p = {
  var_interval = variable_interval p;
  var_rat_bounds = variable_num_interval p;
  constraints = List.map snd p.constraints;
}