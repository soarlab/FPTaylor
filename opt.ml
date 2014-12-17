open Expr
open Environment

let var_bound_float name = 
  variable_interval name

let var_bound_rat name =
  let v = find_variable name in
  v.lo_bound.rational_v, v.hi_bound.rational_v

let optimize tolx tolf e =
  let min, max =
    match Config.opt with
      | "z3" -> Opt_z3.min_max_expr tolf var_bound_rat e
      | "bb" -> 
	let max_iter = Config.get_int_option "bb-iter" (-1) in
	Opt_basic_bb.min_max_expr tolx tolf max_iter var_bound_float e
      | "nlopt" -> Opt_nlopt.min_max_expr tolf var_bound_float e
      | s -> failwith ("Unsupported optimization engine: " ^ s) in
  min, max
