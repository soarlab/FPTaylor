(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with the Z3 SMT solver (see z3opt.py)                         *)
(* -------------------------------------------------------------------------- *)

open Num
open List
open Lib
open Expr
open Opt_common

module Out = ExprOut.Make(ExprOut.Z3PythonPrinter)
       
let gen_z3py_opt_code (pars : Opt_common.opt_pars) fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl() in
  let p' = Format.pp_print_string fmt in

  let head () = 
    p "from z3 import *";
    p "from z3opt import *";
    p "" in

  let tail () =
    p "";
    p (Format.sprintf "fTol = %f" pars.f_abs_tol);
    p (Format.sprintf
         "l, u = find_bounds(f, var_constraints + constraints, fTol, %d)"
	 pars.timeout);
    p "print('{0:.20e}'.format(l))";
    p "print('{0:.20e}'.format(u))" in

  let num_to_z3 n =
    let s = Big_int.string_of_big_int in
    let ns = s (More_num.numerator n) and
	ds = s (More_num.denominator n) in
(*    "\"" ^ ns ^ "/" ^ ds ^ "\"" in*)
    "Q(" ^ ns ^ "," ^ ds ^ ")" in

  let const_to_z3 c =
    if Const.is_rat_const c then num_to_z3 (Const.to_num c)
    else failwith "gen_z3py_opt_code: interval constant" in

  let print_constraint c =
    match c with
    | Le (e, Const c) ->
       Out.print_fmt fmt e;
       p' " < ";
       p' (const_to_z3 c)
    | _ -> failwith "z3opt: print_constraint(): unsupported constraint" in

  let constraint_vars (name, c) =
    match c with
      | Le (e, Const _) -> vars_in_expr e
      | _ -> failwith "z3opt: constraint_vars(): unsupported constraint" in

  let constraints cs =
    if cs = [] then
      p "constraints = []"
    else
      let _ = p' "constraints = [" in
      let _ = map (fun (name, c) -> print_constraint c; p' ", ") cs in
      p "]" in

  let vars var_names var_bounds =
    if var_names = [] then 
      p "var_constraints = []"
    else
      let eq_vars, neq_vars = partition
	(fun (name, (low, high)) -> low =/ high)
	(zip var_names var_bounds) in

      let eqs = map
	(fun (name, (low, high)) ->
	  Format.sprintf "%s = %s" name (num_to_z3 low))
	eq_vars in
      let constraints = map
	(fun (name, (low, high)) ->
	  if low =/ high then
	    Format.sprintf "%s >= %s, %s <= %s" name (num_to_z3 low) name (num_to_z3 high)
	  else
	    Format.sprintf "%s > %s, %s < %s" name (num_to_z3 low) name (num_to_z3 high))
	neq_vars in
      let names =  String.concat ", " var_names in
      p (Format.sprintf "[%s] = Reals('%s')" names names);
      p (String.concat "\n" eqs);
      p (Format.sprintf "var_constraints = [%s]" (String.concat ", " constraints)) in
	

(*
      let low, high = unzip var_bounds in
      let low_str = String.concat ", " (map2 (Format.sprintf "%s > %s") 
					  var_names (map num_to_z3 low)) in
      let high_str = String.concat ", " (map2 (Format.sprintf "%s < %s") 
					   var_names (map num_to_z3 high)) in
      let names =  String.concat ", " var_names in
      p (Format.sprintf "[%s] = Reals('%s')" names names);
      p (Format.sprintf "var_constraints = [%s, %s]" low_str high_str) in
*)

  let expr e = 
    p' "f = ";
    Out.print_fmt fmt e in

  fun (var_bound, e) ->
    let cs = Environment.get_active_constraints() in
    let vars_cs = map constraint_vars cs in
    let var_names = unions (vars_in_expr e :: vars_cs) in
    let var_bounds = map var_bound var_names in
    head ();
    vars (map (fun name -> "var_" ^ name) var_names) var_bounds;
    constraints cs;
    expr e;
    tail ()

let name_counter = ref 0;;

let min_max_expr (pars : Opt_common.opt_pars) var_bound e =
  if vars_in_expr e = [] then
    let n = Eval.eval_num_const_expr e in
    let t = More_num.interval_of_num n in
    (t.Interval.low, t.Interval.high)
  else
    let tmp = Lib.get_tmp_dir () in
    let _ = incr name_counter in
    let py_name = Filename.concat tmp 
      (Format.sprintf "min_max_%d.py" !name_counter) in
    let gen = gen_z3py_opt_code pars in
    let _ = write_to_file py_name gen (var_bound, e) in
    let cmd = Format.sprintf "PYTHONPATH=\"%s\" python %s"
      Config.base_dir py_name in
    let ss = run_cmd cmd in
    let n = length ss in
    let v_min = float_of_string (nth ss (n - 2)) and
	v_max = float_of_string (nth ss (n - 1)) in
    (* Do not add the tolerance: min and max are verified bounds *)
    (v_min, v_max)

