open List
open Num
open Environment

type proof_var = {
  name : string;
  low : num;
  high : num;
}

type proof_op =
  | Proof_var of string
  | Proof_const of num
  (* TODO: all rounding operations *)
  | Proof_rnd of int
  | Proof_neg
  | Proof_add
  | Proof_sub
  | Proof_mul
  | Proof_inv
  | Proof_sqrt
  | Proof_sin
  | Proof_cos

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

type proof = {
  mutable proof_vars : proof_var list;
  mutable proof_steps : proof_step list;
}

let proof = {
  proof_vars = [];
  proof_steps = [];
}

let save_proof fname =
  let tmp = Lib.get_dir "proofs" in
  let name = Filename.concat tmp fname in
  let oc = open_out_bin name in
  output_value oc proof;
  close_out oc

let load_proof fname =
  let ic = open_in_bin fname in
  let p : proof = input_value ic in
  let _ = close_in ic in
  p

let mk_proof_args args errs bounds = {
  arg_indices = args;
  err_indices = errs;
  bounds = bounds;
}

let add_proof_step i op args = 
  let step = {
    step_index = i;
    proof_op = op;
    proof_args = args;
  } in
  proof.proof_steps <- step :: proof.proof_steps

let new_proof () =
  let vars0 = all_variables () in
  let vars = map (fun v -> {
    name = v.var_name;
    low = v.lo_bound.Expr.rational_v;
    high = v.hi_bound.Expr.rational_v;
  }) vars0 in
  proof.proof_vars <- vars;
  proof.proof_steps <- []

let add_var_step i name =
  let op = Proof_var name in
  let args = mk_proof_args [] [] [] in
  add_proof_step i op args
  
let add_const_step i c =
  let op = Proof_const c in
  let args = mk_proof_args [] [] [] in
  add_proof_step i op args

let add_rnd_step i bits arg s1_bound m2_bound =
  let op = Proof_rnd bits in
  let args = mk_proof_args [arg] [] [s1_bound; m2_bound] in
  add_proof_step i op args

let add_neg_step i arg =
  let op = Proof_neg in
  let args = mk_proof_args [] [] [] in
  add_proof_step i op args

let add_add_step i arg1 arg2 =
  let op = Proof_add in
  let args = mk_proof_args [arg1; arg2] [] [] in
  add_proof_step i op args
  
let add_sub_step i arg1 arg2 =
  let op = Proof_sub in
  let args = mk_proof_args [arg1; arg2] [] [] in
  add_proof_step i op args

let add_mul_step i arg1 arg2 m2_bound e2 =
  let op = Proof_mul in
  let args = mk_proof_args [arg1; arg2] [] [m2_bound; e2] in
  add_proof_step i op args

let add_inv_step i arg bound =
  let op = Proof_inv in
  let args = mk_proof_args [arg] [] [bound] in
  add_proof_step i op args

let add_sqrt_step i arg bound =
  let op = Proof_sqrt in
  let args = mk_proof_args [arg] [] [bound] in
  add_proof_step i op args

let add_sin_step i arg bound =
  let op = Proof_sin in
  let args = mk_proof_args [arg] [] [bound] in
  add_proof_step i op args

let add_cos_step i arg bound =
  let op = Proof_cos in
  let args = mk_proof_args [arg] [] [bound] in
  add_proof_step i op args
