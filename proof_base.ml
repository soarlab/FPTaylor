(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Proof certificate data structures                                          *)
(* -------------------------------------------------------------------------- *)

open Num

type proof_var = {
  name : string;
  low : num;
  high : num;
}

(* TODO: all rounding operations *)
type rnd_proof_info = {
  bits : int;
  coefficient : float;
}

type proof_op =
  | Proof_var of string
  | Proof_const of num
  | Proof_rnd_bin_var of rnd_proof_info * string
  | Proof_rnd_bin_const of rnd_proof_info * num
  | Proof_rnd of rnd_proof_info
  | Proof_simpl_eq of int * int
  | Proof_simpl_add of int * int
  | Proof_neg
  | Proof_add
  | Proof_sub
  | Proof_mul
  | Proof_inv
  | Proof_sqrt
  | Proof_sin
  | Proof_cos
  | Proof_atn
  | Proof_exp
  | Proof_log

type proof_opt_type =
  | Proof_opt_approx
  | Proof_opt_exact

type proof_args = {
  arg_indices : int list;
  err_indices : int list;
  bounds : float list;
}

type proof_step = {
  step_index : int;
  proof_op : proof_op;
  proof_args : proof_args;
}

type proof_opt = {
  opt_type : proof_opt_type;
  opt_bounds : float list;
  opt_indices : int list;
  total_bound : float;
}

type proof = {
  mutable proof_vars : proof_var list;
  mutable proof_steps : proof_step list;
  mutable proof_opts : proof_opt list;
}

let proof = {
  proof_vars = [];
  proof_steps = [];
  proof_opts = [];
}

let save_proof dir fname =
  let tmp = Lib.get_dir dir in
  let name = Filename.concat tmp fname in
  let oc = open_out_bin name in
  output_value oc proof;
  close_out oc

let load_proof fname =
  let ic = open_in_bin fname in
  let p : proof = input_value ic in
  let _ = close_in ic in
  p

