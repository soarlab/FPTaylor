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
open Opt_common

(* GELPIA parameters *)
(*
type gelpia_pars = {
  input_epsilon  : float;
  output_epsilon : float;
  timeout        : int;
  max_iters      : int;
}
 *)

let gen_gelpia_code fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl () in
  let p' str = Format.pp_print_string fmt str in

  let parameters pars =
    let timeout = max (pars.timeout / 1000) 1 in
    p (Format.sprintf "-ie %e" pars.x_abs_tol);
    p (Format.sprintf "-oe %e" pars.f_abs_tol);
    p (Format.sprintf "-t %d" timeout);
    (*  p (Format.sprintf "-M %d" pars.max_iters); *)
  (*    p (Format.sprintf "--solver %s" pars.solver) *) in

  let func expr = 
    p' "-f \"";
    print_expr_in_env gelpia_print_env fmt expr;
    p "\"" in

  let input names bounds =
    let dict = List.map2 (fun name b ->
      Format.sprintf "'%s' : (%.20e, %.20e)" name b.low b.high) 
      names bounds in
    p ("-i \"{" ^ String.concat ", " dict ^ "}\"") in

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

let abs_max_expr (pars : Opt_common.opt_pars) var_bound expr =
(*
  let pars = {
    input_epsilon  = tol_x;
    output_epsilon = tol_f;
    timeout        = 30;
    max_iters      = 50000;
  } in
 *)
  let tmp = Lib.get_dir "tmp" in
  let gelpia_name = 
    let _ = incr name_counter in
    Filename.concat tmp 
      (Format.sprintf "gelpia_%d.txt" !name_counter) in
  let gen = gen_gelpia_code in
  let abs_expr = mk_abs expr in
  let _ = write_to_file gelpia_name gen (pars, var_bound, abs_expr) in
  let cmd = Format.sprintf "%s -T -t 5 -z %@%s" (get_gelpia_cmd()) gelpia_name in
  let out = run_cmd cmd in
  try
    let min = get_float out "Minimum: " and
        max = get_float out "Maximum: " in
    min, max
  with _ ->
    let msg = "GELPIA error: " ^ String.concat "\n" (cmd :: out) in
    failwith msg

