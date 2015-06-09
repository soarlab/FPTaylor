(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with the GELPIA tool                                          *)
(* -------------------------------------------------------------------------- *)

open Interval
open List
open Lib
open Expr
open Opt_utils

(* GELPIA parameters *)
type gelpia_pars = {
  input_epsilon : float;
  output_epsilon : float;
  solver : string;
  threads : int;
}

let gen_gelpia_code fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl () in
  let p' str = Format.pp_print_string fmt str in

  let parameters pars =
    p (Format.sprintf "--input-epsilon %f" pars.input_epsilon);
    p (Format.sprintf "--output-epsilon %f" pars.output_epsilon);
    p (Format.sprintf "--threads %d" pars.threads);
    p (Format.sprintf "--solver %s" pars.solver) in

  let func expr = 
    p' "--function \"";
    print_expr_in_env gelpia_print_env fmt expr;
    p "\"" in

  let input names bounds =
    let dict = List.map2 (fun name b ->
      Format.sprintf "'%s' : (%.20e, %.20e)" name b.low b.high) 
      names bounds in
    p ("--input \"{" ^ String.concat ", " dict ^ "}\"") in

  fun (pars, var_bound, expr) ->
    let var_names = vars_in_expr expr in
    let var_bounds = map var_bound var_names in
    parameters pars;
    func expr;
    input var_names var_bounds


let name_counter = ref 0

let get_gelpia_cmd () =
  let cc = Filename.concat in
  let path =
    try
      Sys.getenv "GELPIA_PATH"
    with Not_found ->
      cc Config.base_dir "gelpia" in
  let cmd = cc (cc path "bin") "gelpia" in
  if Sys.file_exists cmd then
    cmd
  else
    failwith (cmd ^ " not found.\n" ^
	      "Set the GELPIA_PATH variable or copy GELPIA to the FPTaylor root directory.")

let abs_max_expr tol_x tol_f var_bound expr =
  let pars = {
    input_epsilon = tol_x;
    output_epsilon = tol_f;
    solver = Config.get_option "gelpia-solver" "map_parallel";
    threads = Config.get_int_option "gelpia-threads" 2;
  } in
  let tmp = Lib.get_dir "tmp" in
  let gelpia_name = 
    let _ = incr name_counter in
    Filename.concat tmp 
      (Format.sprintf "gelpia_%d.txt" !name_counter) in
  let gen = gen_gelpia_code in
  let abs_expr = mk_abs expr in
  let _ = write_to_file gelpia_name gen (pars, var_bound, abs_expr) in
  let cmd = Format.sprintf "%s @%s" (get_gelpia_cmd()) gelpia_name in
  let out = run_cmd cmd in
  try
    let max = get_float out "Maximum: " in
    max
  with _ ->
    let msg = "GELPIA error: " ^ String.concat "\n" (cmd :: out) in
    failwith msg

