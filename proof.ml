(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Proof certificate data structures and constructors                         *)
(* -------------------------------------------------------------------------- *)

open List
open Num
open Environment

include Proof_base

let mk_rnd_info bits c = {
  bits = bits;
  coefficient = c;
}

let mk_proof_args args errs bounds = {
  arg_indices = args;
  err_indices = errs;
  bounds = bounds;
}

let mk_proof_opt opt indices bounds total = {
  opt_type = opt;
  opt_bounds = bounds;
  opt_indices = indices;
  total_bound = total;
}

let add_proof_step i op args = 
  let step = {
    step_index = i;
    proof_op = op;
    proof_args = args;
  } in
  proof.proof_steps <- proof.proof_steps @ [step]

let add_proof_opt opt =
  proof.proof_opts <- proof.proof_opts @ [opt]

let new_proof () =
  let vars0 = all_variables () in
  let vars = map (fun v -> {
    name = v.var_name;
    low = Const.low_bound_to_num v.lo_bound;
    high = Const.high_bound_to_num v.hi_bound;
  }) vars0 in
  proof.proof_vars <- vars;
  proof.proof_steps <- [];
  proof.proof_opts <- []

let add_var_step i name =
  let op = Proof_var name in
  let args = mk_proof_args [] [] [] in
  add_proof_step i op args
  
let add_const_step i c =
  let op = Proof_const c in
  let args = mk_proof_args [] [] [] in
  add_proof_step i op args

let add_rnd_bin_var_step i name rnd p2_exp bound err_index =
  let op = Proof_rnd_bin_var (rnd, name) in
  let args = mk_proof_args [] [err_index] [float_of_int p2_exp; bound] in
  add_proof_step i op args

let add_rnd_bin_const_step i c rnd p2_exp bound err_index =
  let op = Proof_rnd_bin_const (rnd, c) in
  let args = mk_proof_args [] [err_index] [float_of_int p2_exp; bound] in
  add_proof_step i op args

let add_rnd_step i rnd arg s1_bound m2_bound r_index m2_index =
  let op = Proof_rnd rnd in
  let args = mk_proof_args [arg] [r_index; m2_index] [s1_bound; m2_bound] in
  add_proof_step i op args

let add_simpl_eq_step i arg err_i1 err_i2 =
  let op = Proof_simpl_eq (err_i1, err_i2) in
  let args = mk_proof_args [arg] [] [] in
  add_proof_step i op args

let add_simpl_add_step i arg err_i1 err_i2 err_i f e =
  let op = Proof_simpl_add (err_i1, err_i2) in
  let args = mk_proof_args [arg] [err_i] [f; float_of_int e] in
  add_proof_step i op args

let add_neg_step i arg =
  let op = Proof_neg in
  let args = mk_proof_args [arg] [] [] in
  add_proof_step i op args

let add_add_step i arg1 arg2 =
  let op = Proof_add in
  let args = mk_proof_args [arg1; arg2] [] [] in
  add_proof_step i op args
  
let add_sub_step i arg1 arg2 =
  let op = Proof_sub in
  let args = mk_proof_args [arg1; arg2] [] [] in
  add_proof_step i op args

let add_mul_step i arg1 arg2 m2_bound e2 m2_index =
  let op = Proof_mul in
  let args = mk_proof_args [arg1; arg2] [m2_index] [m2_bound; e2] in
  add_proof_step i op args

let add_inv_step i arg m1 m2 e2 b m3 m3_index =
  let op = Proof_inv in
  let args = mk_proof_args [arg] [m3_index] [m1; m2; float_of_int e2; b; m3] in
  add_proof_step i op args

let add_sqrt_step i arg m1 m2 e2 b m3 m3_index =
  let op = Proof_sqrt in
  let args = mk_proof_args [arg] [m3_index] [m1; m2; float_of_int e2; b; m3] in
  add_proof_step i op args

let add_sin_step i arg m1 m2 e2 b m3 m3_index =
  let op = Proof_sin in
  let args = mk_proof_args [arg] [m3_index] [m1; m2; float_of_int e2; b; m3] in
  add_proof_step i op args

let add_cos_step i arg m1 m2 e2 b m3 m3_index =
  let op = Proof_cos in
  let args = mk_proof_args [arg] [m3_index] [m1; m2; float_of_int e2; b; m3] in
  add_proof_step i op args

let add_atn_step i arg m1 m2 e2 b m3 m3_index =
  let op = Proof_atn in
  let args = mk_proof_args [arg] [m3_index] [m1; m2; float_of_int e2; b; m3] in
  add_proof_step i op args

let add_exp_step i arg m1 m2 e2 b m3 m3_index =
  let op = Proof_exp in
  let args = mk_proof_args [arg] [m3_index] [m1; m2; float_of_int e2; b; m3] in
  add_proof_step i op args

let add_log_step i arg m1 m2 e2 b m3 m3_index =
  let op = Proof_log in
  let args = mk_proof_args [arg] [m3_index] [m1; m2; float_of_int e2; b; m3] in
  add_proof_step i op args

let add_opt_approx indices bounds total =
  let opt = Proof_opt_approx in
  add_proof_opt (mk_proof_opt opt indices bounds total)

let add_opt_exact bound e_exp total =
  let opt = Proof_opt_exact in
  add_proof_opt (mk_proof_opt opt [] [bound; float_of_int e_exp] total)
