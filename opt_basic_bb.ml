(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with a simple branch and bound procedure                      *) 
(* (see b_and_b/opt0.ml)                                                      *)
(* -------------------------------------------------------------------------- *)

open Interval
open List
open Lib
open Expr
open Opt_common

let gen_bb_opt_code (pars : Opt_common.opt_pars) max_only fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl() in
(*  let p' = Format.pp_print_string fmt in*)

  let head () = 
    p "open Interval";
    p "open Func";
    p "" in

  let tail_opt0 () =
    p "";
    p "let _ =";
    p (Format.sprintf "  let x_tol = size_max_X start_interval *. %e +. %e in"
                      pars.x_rel_tol pars.x_abs_tol);
    p (Format.sprintf 
	 "  let upper_bound, lower_bound, c = Opt0.opt f_X start_interval x_tol (%e) (%e) (%d) in" 
	 pars.f_rel_tol pars.f_abs_tol pars.max_iters);
    p "  let () = Printf.printf \"iter_max = %d\\n\" c in";
    p "  let () = Printf.printf \"max = %0.20e\\n\" upper_bound in";
    p "  let () = Printf.printf \"lower_max = %0.20e\\n\" lower_bound in";
    if max_only then
      begin
        p "  let () = Printf.printf \"iter_min = 0\\n\" in";
        p "  let () = Printf.printf \"min = 0\\n\" in";
        p "  let () = Printf.printf \"lower_min = 0\\n\" in";
      end
    else
      begin
        p (Format.sprintf 
	     "  let upper_bound, lower_bound, c = Opt0.opt (fun x -> ~-$ (f_X x)) start_interval x_tol (%e) (%e) (%d) in" 
	     pars.f_rel_tol pars.f_abs_tol pars.max_iters);
        p "  let () = Printf.printf \"iter_min = %d\\n\" c in";
        p "  let () = Printf.printf \"min = %0.20e\\n\" (-. upper_bound) in";
        p "  let () = Printf.printf \"lower_min = %0.20e\\n\" (-. lower_bound) in";
      end;
    p "  flush stdout"; in

  let start_interval var_bounds =
    let rec bounds i strs =
      match strs with
	| [] ->
	  p "| _ -> failwith \"Out of boundaries\"";
	| str :: rest ->
	  p (Format.sprintf "| %d -> %s" i str);
	  bounds (i + 1) rest in
    let n = length var_bounds in
    let strs = map 
      (fun b -> Format.sprintf "{low = %.30e; high = %.30e}" b.low b.high) var_bounds in
    nl();
    p (Format.sprintf "let start_interval = Array.init %d (function" n);
    bounds 0 strs;
    p ")";
    nl() in

  let expr var_names e = 
    let rec vars i vs =
      match vs with
	| [] -> ()
	| v :: rest ->
	  p (Format.sprintf "  let var_%s = input_array.(%d) in" v i);
	  vars (i + 1) rest in
    p "let f_x input_array = ";
    vars 0 var_names;
    print_expr_in_env ocaml_float_print_env fmt e;
    nl();
    p "let f_X input_array = ";
    vars 0 var_names;
    print_expr_in_env ocaml_interval_print_env fmt e;
    nl() in

  fun (var_bound, e) ->
    let var_names = vars_in_expr e in
    let var_bounds = map var_bound var_names in
    head();
    start_interval var_bounds;
    expr var_names e;
    match Config.get_string_option "bb-alg" with 
    | "opt0" -> tail_opt0()
    | alg -> failwith (Format.sprintf "Unknown bb algorithm: %s" alg)
 
let counter = ref 0

let min_max_expr (pars : Opt_common.opt_pars) max_only var_bound e =
(*  let _ = counter := !counter + 1 in *)
  let _ = 
    if Config.debug then
      Log.report 4 "bb_opt: x_abs_tol = %e, f_rel_tol = %e, f_abs_tol = %e, iters = %d"
		 pars.x_abs_tol pars.f_rel_tol pars.f_abs_tol pars.max_iters in
  let base = Config.base_dir in
  let tmp = Lib.get_tmp_dir () in
  let ml_name = Filename.concat tmp "bb.ml" in

(*  let ml_name = Format.sprintf "%s/bb_%d.ml" tmp !counter in *)

  let exe_name = Filename.concat tmp "bb" in
  let interval_files_native = map (Filename.concat "INTERVAL") [
    "libinterval.a";
    "interval.cmxa";
  ] in
  let interval_files_byte = map (Filename.concat "INTERVAL") [
    "chcw.o";
    "interval.cma";
  ] in
  let lib_files = [
    "func.ml";
  ] in
  let bb_files = map (Filename.concat "b_and_b") [
    "opt0.ml";
  ] in
  let ocamlc = Config.get_string_option "bb-ocamlc" in
  let files = 
    if ocamlc = "ocamlc" then
      interval_files_byte @ lib_files @ bb_files
    else
      interval_files_native @ lib_files @ bb_files in
  let gen = gen_bb_opt_code pars max_only in
  let _ = write_to_file ml_name gen (var_bound, e) in
  let srcs = map (fun s -> Filename.concat base s) files in
  let cmd = Format.sprintf "%s -I %s -I %s -I %s -o %s %s %s" 
    ocamlc
    base
    (Filename.concat base "INTERVAL")
    (Filename.concat base "b_and_b")
    exe_name
    (String.concat " " srcs) 
    ml_name in
  let _ = run_cmd cmd in
  let out = run_cmd exe_name in
  let fmin = Opt_common.get_float out "min = " and
      fmax = Opt_common.get_float out "max = " in
  let _ = 
    if Config.debug then
      let iter_max = truncate (Opt_common.get_float out "iter_max = ") and
	  iter_min = truncate (Opt_common.get_float out "iter_min = ") and
	  lower_max = Opt_common.get_float out "lower_max = " and
	  lower_min = Opt_common.get_float out "lower_min = " in
      let opt, subopt =
	if abs_float fmin > abs_float fmax then
	  abs_float fmin, abs_float (fmin -. lower_min)
	else
	  abs_float fmax, abs_float (fmax -. lower_max) in
      Log.report 4 "iterations(%d, %d): %d" 
		 iter_max iter_min (max iter_max iter_min);
      Log.report 4 "lower_max = %e, lower_min = %e, subopt = %e (%e)"
		 lower_max lower_min subopt (subopt /. opt *. 100.0)
    else () in
  fmin, fmax
