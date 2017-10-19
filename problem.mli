type var_info = {
  var_name : string;
  var_type : Rounding.value_type;
  lo_bound : Const.t;
  hi_bound : Const.t;
  uncertainty : Const.t;
}

type problem = {
  name : string;
  expression : Expr.expr;
  variables : var_info list;
  constraints : (string * Expr.formula) list;
}

val find_variable : problem -> string -> var_info

val variable_type : problem -> string -> Rounding.value_type

val variable_interval : problem -> string -> Interval.interval

val variable_num_interval : problem -> string -> Num.num * Num.num

val constraints_of_problem : problem -> Expr.constraints
