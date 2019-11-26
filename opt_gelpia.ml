(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with the GELPIA tool                                          *)
(* -------------------------------------------------------------------------- *)

open Interval
open Expr
open Opt_common

module Out = ExprOut.Make(ExprOut.GelpiaPrinter)

(* GELPIA parameters *)
(*
type gelpia_pars = {
  x_abs_tol      : float;
  f_rel_tol      : float;
  f_abs_tol      : float;
  timeout        : int;
  iters          : int;
}
 *)

let gen_gelpia_code fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl () in

  let parameters pars x_tol =
    let timeout = max (pars.timeout / 1000) 1 in
    p (Format.sprintf "# --input-epsilon %e" x_tol);
    p (Format.sprintf "# --output-epsilon %e" pars.f_abs_tol);
    p (Format.sprintf "# --output-epsilon-relative %e" pars.f_rel_tol);
    p (Format.sprintf "# --timeout %d" timeout);
    p (Format.sprintf "# --max-iters %d" pars.max_iters); in

  let func expr names bounds=
    let vars = List.map2 (fun name b ->
                   Format.sprintf "%s = [%.20e, %.20e];\n" (ExprOut.fix_name name) b.low b.high)
                 names bounds in
    p (String.concat "" vars);
    Out.print_fmt fmt expr; in

  fun (pars, cs, expr) ->
    let var_names = vars_in_expr expr in
    let var_bounds = List.map cs.var_interval var_names in
    let domain_size = Lib.itlist (fun b r -> max r (abs_float (b.high -. b.low)))
                                 var_bounds 0.0 in
    let x_tol = domain_size *. pars.x_rel_tol +. pars.x_abs_tol in
    parameters pars x_tol;
    func expr (List.map (fun name -> "var_" ^ name) var_names) var_bounds


let name_counter = ref 0

let get_gelpia_cmd ()=
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

let min_max_expr (pars : Opt_common.opt_pars) max_only (cs : constraints) expr =
(*
  let pars = {
    input_epsilon  = tol_x;
    output_epsilon = tol_f;
    timeout        = 30;
    max_iters      = 50000;
  } in
 *)
  let tmp = Lib.get_tmp_dir () in
  let gelpia_name =
    incr name_counter;
    Filename.concat tmp
      (Format.sprintf "gelpia_%d.txt" !name_counter) in
  let gen = gen_gelpia_code in
  let abs_expr = expr in
  let _ = Lib.write_to_file gelpia_name gen (pars, cs, abs_expr) in
  let cmd = Format.sprintf "%s --mode=%s %s" (get_gelpia_cmd ()) (if max_only then "max" else "min-max") gelpia_name in
  let out = Lib.run_cmd cmd in
  try
    let min = if max_only then 0.0 else get_float out "Minimum lower bound " in
    let max = get_float out "Maximum upper bound " in
    min, max
  with _ ->
    let msg = "GELPIA error: " ^ String.concat "\n" (cmd :: out) in
    failwith msg
