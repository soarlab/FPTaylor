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

module Out = ExprOut.Make(ExprOut.OCamlIntervalPrinter)
(* module OutFloat = ExprOut.Make(ExprOut.OCamlFloatPrinter) *)
       
let gen_bb_opt_code (pars : Opt_common.opt_pars) max_only fmt =
  let nl = Format.pp_print_newline fmt in
  let p str = Format.pp_print_string fmt str; nl() in
(*  let p' = Format.pp_print_string fmt in*)

  let head () = 
    p "open Interval";
    p "open Opt_func";
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
      (fun b -> Format.sprintf "{low = %.20e; high = %.20e}" b.low b.high) var_bounds in
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
(*    p "let f_x input_array = ";
    vars 0 var_names;
    OutFloat.print_fmt fmt e;
    nl(); *)
    p "let f_X input_array = ";
    vars 0 var_names;
    Out.print_fmt fmt e;
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
  if Config.debug then
    Log.report 4 "bb_opt: x_abs_tol = %e, f_rel_tol = %e, f_abs_tol = %e, iters = %d"
	       pars.x_abs_tol pars.f_rel_tol pars.f_abs_tol pars.max_iters;
  let base = Config.base_dir in
  let tmp = Lib.get_tmp_dir () in
  let in_name = 
    let name = incr counter; Format.sprintf "bb_%d.ml" !counter in
    Filename.concat tmp name in
  let out_name = Filename.concat tmp "bb" in
  let gen = gen_bb_opt_code pars max_only in
  let () = write_to_file in_name gen (var_bound, e) in
  let cmd =
    let quote s = "\"" ^ s ^ "\"" in
    let str = Config.get_string_option "bb-compile" in
    let str = Str.global_replace (Str.regexp "{base}") (quote base) str in
    let str = Str.global_replace (Str.regexp "{out}") (quote out_name) str in
    let str = Str.global_replace (Str.regexp "{input}") (quote in_name) str in
    str in
  let _ = run_cmd cmd in
  let out = run_cmd out_name in
  let fmin = Opt_common.get_float out "min = " and
      fmax = Opt_common.get_float out "max = " and
      iter_max = truncate (Opt_common.get_float out ~default:0. "iter_max = ") and
      iter_min = truncate (Opt_common.get_float out ~default:0. "iter_min = ") and
      lower_max = Opt_common.get_float out ~default:neg_infinity "lower_max = " and
      lower_min = Opt_common.get_float out ~default:infinity "lower_min = " in
  let rmin = {
      result = fmin;
      lower_bound = lower_min;
      iters = iter_min;
      time = 0.;
    } in
  let rmax = {
      result = fmax;
      lower_bound = lower_max;
      iters = iter_max;
      time = 0.;
    } in
  if Config.debug then begin
      let opt, subopt =
        if abs_float fmin > abs_float fmax then
	  abs_float fmin, abs_float (fmin -. lower_min)
        else
	  abs_float fmax, abs_float (fmax -. lower_max) in
      Log.report 4 "iterations(min = %d, max = %d): %d" 
	         iter_min iter_max (max iter_max iter_min);
      Log.report 4 "min = %e (lower_min = %e)" fmin lower_min;
      Log.report 4 "max = %e (lower_max = %e)" fmax lower_max;
      Log.report 4 "subopt = %e (%.1f%%)" subopt (subopt /. opt *. 100.0)
    end;
  rmin, rmax
