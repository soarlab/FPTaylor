(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Optimization with a simple branch and bound procedure                      *) 
(* (see b_and_b/opt0.ml)                                                      *)
(* -------------------------------------------------------------------------- *)

open Interval
open List
open Lib
open Expr
open Opt_utils

let gen_bb_opt_code tolx tolfx max_iter fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl() in
(*  let p' = Format.pp_print_string fmt in*)

  let head () = 
    p "open Interval";
    p "open Fp";
    p "" in

  let tail_b_and_b () =
    p "";
    p "let _ =";
    p (Format.sprintf 
	 "  let int, fint, p, pv = B_and_b.branch_and_bound f_x f_X start_interval %f %f in"
	 tolx tolfx);
    p "  let _ = print_I fint; print_newline ();";
    p "          Printf.printf \"max = %0.20e\\n\" fint.high in";
    p (Format.sprintf
	 "  let int, fint, p, pv = B_and_b.branch_and_bound (fun x -> -. (f_x x)) (fun x -> ~-$ (f_X x)) start_interval %f %f in" tolx tolfx);
    p "  let _ = print_I fint; print_newline ();";
    p "          Printf.printf \"min = %0.20e\\n\" (-. fint.high) in";
    p "  flush stdout"; in

  let tail_opt0 () =
    p "";
    p "let _ =";
    p (Format.sprintf 
	 "  let m, bound, c = Opt0.opt f_X start_interval %f %f (%d) in" 
	 tolx tolfx max_iter);
    p "  let _ = Printf.printf \"iter_max = %d\\n\" c in";
    p "  let _ = Printf.printf \"max = %0.20e\\n\" m in";
    p (Format.sprintf 
	 "  let m, bound, c = Opt0.opt (fun x -> ~-$ (f_X x)) start_interval %f %f (%d) in" 
	 tolx tolfx max_iter);
    p "  let _ = Printf.printf \"iter_min = %d\\n\" c in";
    p "  let _ = Printf.printf \"min = %0.20e\\n\" (-. m) in";
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
    if Config.get_option "bb-alg" "opt0" = "opt0" then
      tail_opt0()
    else
      tail_b_and_b()

let counter = ref 0

let min_max_expr tolx tolfx max_iter var_bound e =
(*  let _ = counter := !counter + 1 in *)
  let _ = 
    if Config.debug then
      Log.report (Format.sprintf "bb_opt: tolx = %e, tolf = %e, iters = %d"
		    tolx tolfx max_iter) in
  let base = Config.base_dir in
  let tmp = Lib.get_dir "tmp" in
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
  let bb_files = map (Filename.concat "b_and_b") [
    "opt0.ml";
    "fp.ml";
  ] in
  let ocamlc = Config.get_option "bb-ocamlc" "ocamlopt" in
  let files = 
    if ocamlc = "ocamlc" then
      interval_files_byte @ bb_files
    else
      interval_files_native @ bb_files in
  let gen = gen_bb_opt_code tolx tolfx max_iter in
  let _ = write_to_file ml_name gen (var_bound, e) in
  let srcs = map (fun s -> Filename.concat base s) files in
  let cmd = Format.sprintf "%s -I %s -I %s -o %s %s %s" 
    ocamlc
    (Filename.concat base "INTERVAL")
    (Filename.concat base "b_and_b")
    exe_name
    (String.concat " " srcs) 
    ml_name in
  let _ = run_cmd cmd in
  let out = run_cmd exe_name in
  let fmin = get_float out "min = " and
      fmax = get_float out "max = " in
  let _ = 
    if Config.debug then
      let iter_max = truncate (get_float out "iter_max = ") and
	  iter_min = truncate (get_float out "iter_min = ") in
      Log.report (Format.sprintf "iterations(%d, %d): %d" 
		    iter_max iter_min (max iter_max iter_min))
    else () in
  fmin, fmax
