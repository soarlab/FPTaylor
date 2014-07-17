open List
open Lib
open Expr

let gen_z3py_opt_code fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl() in
  let p' = Format.pp_print_string fmt in

  let head () = 
    p "from z3 import *";
    p "from z3opt import *";
    p "" in

  let tail tol =
    p "";
    p (Format.sprintf "fTol = %f" tol);
    p "l, u = find_bounds(f, constraints, fTol)";
    p "print l";
    p "print u" in

  let num_to_z3 n =
    let s = Big_int.string_of_big_int in
    let ns = s (More_num.numerator n) and
	ds = s (More_num.denominator n) in
(*    "\"" ^ ns ^ "/" ^ ds ^ "\"" in*)
    "Q(" ^ ns ^ "," ^ ds ^ ")" in 

  let vars var_names var_bounds =
    if var_names = [] then 
      p "constraints = []"
    else
      let low, high = unzip var_bounds in
      let low_str = String.concat ", " (map2 (Format.sprintf "%s >= %s") 
					  var_names (map num_to_z3 low)) in
      let high_str = String.concat ", " (map2 (Format.sprintf "%s <= %s") 
					   var_names (map num_to_z3 high)) in
      let names =  String.concat ", " var_names in
      p (Format.sprintf "[%s] = Reals('%s')" names names);
      p (Format.sprintf "constraints = [%s, %s]" low_str high_str) in

  let expr e = 
    p' "f = ";
    print_expr_in_env z3py_print_env fmt e in

  fun (tol, var_bound, e) ->
    let var_names = vars_in_expr e in
    let var_bounds = map var_bound var_names in
    head();
    vars var_names var_bounds;
    expr e;
    tail tol


let min_max_expr tol var_bound e =
  if vars_in_expr e = [] then
    let n = Eval.eval_num_const_expr e in
    let t = More_num.interval_of_num n in
    (t.Interval.low, t.Interval.high)
  else
    let tmp = Lib.get_dir "tmp" in
    let py_name = Filename.concat tmp "min_max.py" in
    let gen = gen_z3py_opt_code in
    let _ = write_to_file py_name gen (tol, var_bound, e) in
    let cmd = Format.sprintf "PYTHONPATH=\"%s\" python %s"
      Config.base_dir py_name in
    let ss = run_cmd cmd in
    let n = length ss in
    let v_min = float_of_string (nth ss (n - 2)) and
	v_max = float_of_string (nth ss (n - 1)) in
    (* Do not add the tolerance: min and max are verified bounds *)
    (v_min, v_max)

