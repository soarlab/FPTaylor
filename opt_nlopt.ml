(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with the nlopt library                                        *)
(* -------------------------------------------------------------------------- *)

open Interval
open List
open Lib
open Expr
open Opt_utils

(* nlopt C/C++ code generator *)
type nlopt_pars = {
  nl_alg : int;
  nl_ftol_abs : float;
  nl_ftol_rel : float;
  nl_xtol_abs : float;
  nl_xtol_rel : float;
  nl_maxeval : int;
};;

let nlopt_default = {
  nl_alg = 0;
  nl_ftol_abs = 1e-3;
  nl_ftol_rel = 0.0;
  nl_xtol_abs = 0.0;
  nl_xtol_rel = 0.0;
  nl_maxeval = 20000;
};;


let gen_nlopt_code pars fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl () in
(*  let p' str = Format.pp_print_string fmt str in*)

  let head () = 
    p "#include <stdio.h>";
    p "#include <math.h>";
    p "#include <nlopt.h>";
    p "" in

  let gen_nlopt_func vars expr =
    let grad() = () in
    let rec init_vars vs n = 
      match vs with
	| [] -> ()
	| h :: t ->
	  p (Format.sprintf "  double %s = _x[%d];" h n);
	  init_vars t (n + 1) in
    p "double f(unsigned _n, const double *_x, double *_grad, void *_f_data) {";
    init_vars vars 0;
    p "  double _result = ";
    print_expr_in_env c_print_env fmt expr;
    p ";";
    p "  if (_grad) {";
    grad();
    p "  }";
    p "  return _result;";
    p "}" in

  let str_of_array vs =
    let ss = map string_of_float vs in
    "{" ^ String.concat "," ss ^ "}" in

  let options var_bounds =
    let ls, us = unzip (map (fun b -> b.low, b.high) var_bounds) in
    let ms = map2 (fun l u -> (l +. u) /. 2.0) ls us in
    p "  // Bounds";
    p (Format.sprintf "  nlopt_set_lower_bounds(opt, (double[])%s);" (str_of_array ls));
    p (Format.sprintf "  nlopt_set_upper_bounds(opt, (double[])%s);" (str_of_array us));
    p "  // Stopping criteria";
    p (Format.sprintf "  nlopt_set_ftol_abs(opt, %f);" pars.nl_ftol_abs);
    p (Format.sprintf "  nlopt_set_maxeval(opt, %d);" pars.nl_maxeval);
    p "  // x0";
    p ("  double x_min[] = " ^ str_of_array ms ^ ";");
    p ("  double x_max[] = " ^ str_of_array ms ^ ";") in

  let main var_names var_bounds =
    p "int main() {";
    p (Format.sprintf "  nlopt_opt opt = nlopt_create(%d, %d);" 
	 pars.nl_alg (length var_names));
    options var_bounds;
    p "  double f_min = 0.0, f_max = 0.0;";
    p "  // min";
    p "  nlopt_set_min_objective(opt, f, NULL);";
    p "  nlopt_result result_min = nlopt_optimize(opt, x_min, &f_min);";
    p "  // max";
    p "  nlopt_set_max_objective(opt, f, NULL);";
    p "  nlopt_result result_max = nlopt_optimize(opt, x_max, &f_max);";
    p "  printf(\"result_min: %d\\n\", result_min);";
    p "  printf(\"result_max: %d\\n\", result_max);";
    p "  printf(\"min: %.30e\\n\", f_min);";
    p "  printf(\"max: %.30e\\n\", f_max);";
    p "  return 0;";
    p "}" in

  fun (var_bound, expr) ->
    let var_names = vars_in_expr expr in
    let var_bounds = map var_bound var_names in
    head();
    gen_nlopt_func var_names expr;
    main var_names var_bounds

    
let min_max_expr ftol var_bound expr =
  let tmp = Lib.get_dir "tmp" in
  let c_name = Filename.concat tmp "nlopt-f.c" in
  let exe_name = Filename.concat tmp "nlopt-f" in
  let gen = gen_nlopt_code {nlopt_default with nl_ftol_abs = ftol} in
  let _ = write_to_file c_name gen (var_bound, expr) in
  let cc = Config.get_string_option "nlopt-cc" in
  let cc_lib = Config.get_string_option "nlopt-lib" in
  let cmd = Format.sprintf "%s -o %s %s %s" cc exe_name c_name cc_lib in
  let out = run_cmd cmd in
  if out <> [] then
    let str = "Compilation ERROR: " ^ String.concat "\n" (cmd :: out) in
    failwith str
  else
    let out = run_cmd exe_name in
    let min = get_float out "min: " and
	max = get_float out "max: " in
    min, max

