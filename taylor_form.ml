(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Symbolic Taylor forms                                                      *)
(* -------------------------------------------------------------------------- *)

open Interval
open Num
open List
open Lib
open Rounding
open Binary_float
open Expr

(* Describes an error variable *)
type error_info = {
  (* Unique index for proofs *)
  proof_index : int;
  (* Error variables with the same index are the same *)
  index : int;
  (* The upper bound of the error is 2^exp *)
  exp : int;
}

type taylor_form = {
  form_index : int;
  v0 : expr;
  v1 : (expr * error_info) list;
}

let dummy_tform = {
  form_index = 0;
  v0 = const_0;
  v1 = [];
}

let reset_form_index, next_form_index =
  let index = ref 0 in
  let reset () = index := 0 in
  let next () =
    let i = !index in
    let _ = incr index in
    i in
  reset, next

let mk_err_var, reset_error_index =
  let err_index = ref 0 in
  let mk index exp = {
    proof_index = (let _ = incr err_index in !err_index);
    index = index;
    exp = exp;
  } in
  let reset () = err_index := 0 in
  mk, reset

let mk_proof_rnd_info rnd =
  Proof.mk_rnd_info rnd.fp_type.bits rnd.coefficient

let ( +^ ) = Fpu.fadd_high and
    ( *^ ) = Fpu.fmul_high

let make_stronger f =
  if Config.proof_flag then
    if f = 0.0 then
      0.0
    else
      More_num.next_float f
  else
    f

let estimate_expr, reset_estimate_cache =
  let cache = ref [] in
  let reset () = (cache := []) in
  let estimate vars e =
    if Config.get_bool_option "intermediate-opt" then
      let () = Log.report 4 "Estimating: %s" (print_expr_str e) in
      let min, max = Opt.find_min_max Opt_common.default_opt_pars e in
      Log.report 4 "Estimation result: [%f, %f]" min max;
      {low = min; high = max}
    else
      Eval.eval_interval_expr vars e in
  let estimate_and_cache vars e =
    try
      assoc_eq eq_expr e !cache
    with Failure _ ->
      let interval = estimate vars e in
      let _ = (cache := (e, interval) :: !cache) in
      interval
  in
  estimate_and_cache, reset

let add2 (x1, e1) (x2, e2) =
  (* Swap if e1 > e2 *)
  let x1, e1, x2, e2 = if e1 <= e2 then x1, e1, x2, e2 else x2, e2, x1, e1 in
  if e1 = 0 then 
    (if e2 = 0 then (0.0, 0) else (x2, e2))
  else if e2 = 0 then 
    (x1, e1)
  else if e1 = e2 then
    (x1 +^ x2, e1)
  else
    let eps = get_eps (e1 - e2) in
    ((x1 *^ eps) +^ x2, e2)

let sum_high s = itlist add2 s (0.0, 0)

let sum2_high s1 s2 = itlist 
  (fun (x,x_exp) s ->
    let s0 = sum_high (map (fun (y,y_exp) -> x *^ y, x_exp + y_exp) s2) in
    add2 s s0) 
  s1 (0.0, 0)

let abs_eval vars ex = 
(*  let v = Eval.eval_interval_expr vars ex in *)
  let v = estimate_expr vars ex in
  (abs_I v).high

let abs_eval_v1 vars = map (fun (ex, err) -> abs_eval vars ex, err.exp)

let fp_to_const f = mk_const (const_of_float f)

let simplify_form vars f =
  let rec add_adjacent arg_index s =
    match s with
      | (ex1, err1) :: (ex2, err2) :: t when err1.index = -1 && err2.index = -1 ->
	let f1, exp1 = abs_eval vars ex1, err1.exp and
	    f2, exp2 = abs_eval vars ex2, err2.exp in
	let f, exp = add2 (f1, exp1) (f2, exp2) in
	let f = make_stronger f in
	let i = next_form_index() in
	let err = mk_err_var (-1) exp in
	let _ = Proof.add_simpl_add_step i arg_index 
	  err1.proof_index err2.proof_index err.proof_index f exp in
	add_adjacent i ((fp_to_const f, err) :: t)
      | (e1, err1) :: (e2, err2) :: t ->
	if err1.index = err2.index then
	  (* we must have err1.exp = err2.exp here *)
	  let _ = assert(err1.exp = err2.exp) in
	  let i = next_form_index() in
	  let _ = Proof.add_simpl_eq_step i arg_index 
	    err1.proof_index err2.proof_index in
	  add_adjacent i ((mk_add e1 e2, err1) :: t)
	else
	  let index, s = add_adjacent arg_index ((e2, err2) :: t) in
	  index, (e1, err1) :: s
      | _ -> arg_index, s in
  let v1 = sort (fun (_, err1) (_, err2) -> compare err1.index err2.index) f.v1 in
  let i, v1_new = add_adjacent f.form_index v1 in
  {f with form_index = i; v1 = v1_new}

let find_index, expr_for_index, reset_index_counter, current_index =
  let counter = ref 0 in
  let exprs = ref [] in
  let find_index expr =
    let unique_flag = Config.get_bool_option "unique-indices" in
    let i = assocd_eq eq_expr (-1) expr !exprs in
    if i > 0 && (not unique_flag) then i else
      let _ = counter := !counter + 1 in
      let _ = exprs := (expr, !counter) :: !exprs in
      !counter 
  and expr_for_index i = rev_assoc i !exprs
  and reset_index_counter () = exprs := []; counter := 0
  and current_index () = !counter
  in
  find_index, expr_for_index, reset_index_counter, current_index

(* constant *)
let const_form e = 
  Log.report 3 "const_form";
  match e with
    | Const c -> 
      let i = next_form_index() in
      let _ = Proof.add_const_step i c.rational_v in {
	form_index = i;
	v0 = e;
	v1 = []
      }
    | _ -> failwith ("const_form: not a constant" ^ print_expr_str e)

(* Constant with rounding *)
(* TODO: subnormal numbers are incorrectly handled by bin_float_of_num *)
let precise_const_rnd_form rnd e =
  Log.report 3 "precise_const_rnd_form";
  match e with
    | Const c ->
      let x = bin_float_of_num (-rnd.eps_exp) rnd.rnd_type c.rational_v in
      let rc = num_of_bin_float x in
      let d = c.rational_v -/ rc in
      (* exact fp number *)
      if d =/ Int 0 then
	const_form e
      else
	let form_index = next_form_index() in
	let err_expr = mk_const (const_of_num (d // (Int 2 **/ Int rnd.eps_exp))) in
(*	let err = mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp in *)
	(* Exact errors for constants can cancel each other: 
	   use the same artificial constant (const_0) for indices *)
	let err = mk_err_var (find_index (mk_rounding rnd const_0)) rnd.eps_exp in
	Log.report 4 "Inexact constant: %s; err = %s"
		   (print_expr_str e)
		   (print_expr_str err_expr);
	{
	  form_index = form_index;
	  v0 = e;
	  v1 = [err_expr, err]
	}
    | _ -> failwith ("precise_const_rnd_form: not a constant: " ^ print_expr_str e)
	

(* constant with rounding *)
let const_rnd_form rnd e =
  Log.report 3 "const_rnd_form";
  if Config.get_bool_option "fp-power2-model" then
    precise_const_rnd_form rnd e
  else
  match e with
    | Const c -> 
      if is_fp_exact (get_eps rnd.eps_exp) c then 
	const_form e
      else
	let form_index = next_form_index() in
	let bound = (abs_I c.interval_v).high in
	let p2 = Func.floor_power2 bound in
	let _, p2_exp = frexp p2 in
	let m2' =
	  (* TODO: do not add d for constants in the normal range *)
	  if Config.proof_flag then
	    p2 +^ (get_eps rnd.delta_exp /. get_eps rnd.eps_exp)
	  else
	    p2 in
	(* Don't need to apply make_stronger since 
	   everything can be proved with rational numbers *)
	let m2 = rnd.coefficient *^ m2' in
	let err_expr = fp_to_const m2 in
	let err = mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp in
	Log.report 4 "Inexact constant: %s; err = %s" 
		   (print_expr_str e) 
		   (print_expr_str err_expr);
	let _ = 
	  Proof.add_rnd_bin_const_step form_index c.rational_v 
	    (mk_proof_rnd_info rnd) p2_exp m2 err.proof_index in
	{
	  form_index = form_index;
	  v0 = e;
	  v1 = [err_expr, err]
	}
    | _ -> failwith ("const_rnd_form: not a constant: " ^ print_expr_str e)

let get_var_uncertainty eps_exp var_name =
  let v = Environment.find_variable var_name in
  let u = v.Environment.uncertainty.rational_v // More_num.num_of_float (get_eps eps_exp) in
  if not (u =/ Int 0) then
    [mk_const (const_of_num u), mk_err_var (find_index (mk_var (var_name ^ "$uncertainty"))) eps_exp]
  else 
    []

(* variable *)
let var_form e =
  Log.report 3 "var_form";
  match e with
    | Var v -> 
      let i = next_form_index() in
      let _ = Proof.add_var_step i v in {
	form_index = i;
	v0 = e;
      (* FIXME: what is the right value of eps_exp here? *)
	v1 = if Config.get_bool_option "uncertainty" then get_var_uncertainty (-53) v else [];
      }
    | _ -> failwith ("var_form: not a variable" ^ print_expr_str e)

(* variable with rounding *)
let var_rnd_form rnd e =
  Log.report 3 "var_rnd_form";
  match e with
    | Var v -> 
      if Config.proof_flag then
	let form_index = next_form_index() in
	let bound = (abs_I (Environment.variable_interval v)).high in
	let p2 = Func.floor_power2 bound in
	let _, p2_exp = frexp p2 in
	let m2' =
	    p2 +^ (get_eps rnd.delta_exp /. get_eps rnd.eps_exp) in
	(* Don't need to apply make_stronger since 
	   everything can be proved with rational numbers *)
	let m2 = rnd.coefficient *^ m2' in
	let err_expr = fp_to_const m2 in
	let err = mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp in
	let _ = 
	  Proof.add_rnd_bin_var_step form_index v 
	    (mk_proof_rnd_info rnd) p2_exp m2 err.proof_index in
	{
	  form_index = form_index;
	  v0 = e;
	  v1 = [err_expr, err]
	}
      else
	let v1_uncertainty = 
	  if Config.get_bool_option "uncertainty" then get_var_uncertainty rnd.eps_exp v else [] in
	let v1_rnd =
	  let err_expr0 = 
	    if Config.get_bool_option "const-approx-real-vars" then
	      let bound = (abs_I (Environment.variable_interval v)).high in
	      let err = Func.floor_power2 bound in
	      fp_to_const err
	    else if Config.get_bool_option "fp-power2-model" then
	      mk_floor_power2 e
	    else
	      e in
	  let err_expr = 
	    if rnd.coefficient <> 1.0 then
	      mk_mul (fp_to_const rnd.coefficient) err_expr0 
	    else 
	      err_expr0 in
	(* TODO: subnormal values of variables *)
	  [err_expr, mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp] in
	{
	  form_index = next_form_index();
	  v0 = e;
	  v1 = v1_uncertainty @ v1_rnd;
	}
    | _ -> failwith ("var_rnd_form: not a variable" ^ print_expr_str e)

(* rounding *)
let rounded_form vars original_expr rnd f =
  Log.report 3 "rounded_form";
  if rnd.eps_exp = 0 then {
    form_index = next_form_index();
    v0 = f.v0;
    v1 = f.v1 @ [fp_to_const rnd.coefficient, mk_err_var (-1) rnd.delta_exp]
  }
  else
    let i = find_index original_expr in
    let s1', exp1 = sum_high (abs_eval_v1 vars f.v1) in
    let s1 = make_stronger (get_eps exp1 *^ s1') in
    let r', m2' =
      if Config.get_bool_option "fp-power2-model" then
	mk_floor_power2 (mk_add f.v0 (mk_sym_interval (fp_to_const s1))),
	get_eps rnd.delta_exp /. get_eps rnd.eps_exp
      else
	f.v0, 
	s1 +^ (get_eps rnd.delta_exp /. get_eps rnd.eps_exp) in
    let r, m2 =
      if rnd.coefficient = 1.0 then
	r', m2'
      else
	mk_mul (fp_to_const rnd.coefficient) r', rnd.coefficient *^ m2' in
    let form_index = next_form_index() in
    let m2 = make_stronger m2 in
    let r_err = mk_err_var i rnd.eps_exp and
	m2_err = mk_err_var (-1) rnd.eps_exp in
    let _ = Proof.add_rnd_step form_index (mk_proof_rnd_info rnd)
      f.form_index s1 m2 r_err.proof_index m2_err.proof_index in
    {
      form_index = form_index;
      v0 = f.v0;
      v1 = ((r, r_err) :: f.v1) @ 
	[fp_to_const m2, m2_err];
    }

(* negation *)
let neg_form f = 
  Log.report 3 "neg_form";
  let i = next_form_index() in
  let _ = Proof.add_neg_step i f.form_index in {
    form_index = i;
    v0 = mk_neg f.v0;
    v1 = map (fun (e, err) -> mk_neg e, err) f.v1;
  }

(* addition *)
let add_form f1 f2 = 
  Log.report 3 "add_form";
  let i = next_form_index() in
  let _ = Proof.add_add_step i f1.form_index f2.form_index in {
    form_index = i;
    v0 = mk_add f1.v0 f2.v0;
    v1 = f1.v1 @ f2.v1;
  }

(* subtraction *)
let sub_form f1 f2 =
  Log.report 3 "sub_form";
  let i = next_form_index() in
  let _ = Proof.add_sub_step i f1.form_index f2.form_index in {
    form_index = i;
    v0 = mk_sub f1.v0 f2.v0;
    v1 = f1.v1 @ map (fun (e, err) -> mk_neg e, err) f2.v1;
  }

(* rounded subtraction *)
let rounded_sub_form vars original_expr rnd f1 f2 =
  Log.report 3 "rounded_sub_form";
  let i = find_index original_expr in
  let s1', exp1 = sum_high (abs_eval_v1 vars f1.v1) in
  let s2', exp2 = sum_high (abs_eval_v1 vars f2.v1) in
  let s1 = make_stronger (get_eps exp1 *^ s1') in
  let s2 = make_stronger (get_eps exp2 *^ s2') in
  let r' = mk_floor_sub2 (mk_add f1.v0 (mk_sym_interval (fp_to_const s1)))
                         (mk_add f2.v0 (mk_sym_interval (fp_to_const s2))) in
  let r = 
    if rnd.coefficient = 1.0 then
      r'
    else
      mk_mul (fp_to_const rnd.coefficient) r' in
  let form_index = next_form_index() in
  let r_err = mk_err_var i rnd.eps_exp in
  {
    form_index = form_index;
    v0 = mk_sub f1.v0 f2.v0;
    v1 = (r, r_err) :: (f1.v1 @ map (fun (e, err) -> mk_neg e, err) f2.v1);
  }
  
(* rounded addition *)
let rounded_add_form vars original_expr rnd f1 f2 =
  Log.report 3 "rounded_add_form";
  rounded_sub_form vars original_expr rnd f1 (neg_form f2)


(* multiplication *)
let mul_form =
  let mul1 x = map (fun (e, err) -> mk_mul x e, err) in
  fun vars f1 f2 -> 
    Log.report 3 "mul_form";
    let x1 = abs_eval_v1 vars f1.v1 and
	y1 = abs_eval_v1 vars f2.v1 in
    let m2, m2_exp = sum2_high x1 y1 in
    let m2 = make_stronger m2 in
    let form_index = next_form_index() in
    let m2_err = mk_err_var (-1) m2_exp in
    let _ = Proof.add_mul_step form_index
      f1.form_index f2.form_index m2 (float_of_int m2_exp) m2_err.proof_index in
    {
      form_index = form_index;
      v0 = mk_mul f1.v0 f2.v0;
      v1 = mul1 f1.v0 f2.v1 @ mul1 f2.v0 f1.v1 @ [fp_to_const m2, m2_err];
    }

(* reciprocal *)
let inv_form vars f = 
  Log.report 3 "inv_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    if Config.proof_flag then
      pow_I_i (x0_int +$ {low = -.m1; high = m1}) 3
    else
      pow_I_i (x0_int +$ s1) 3 in
  let _ = 
    if (abs_I d).low <= 0.0 then
      let msg = "inv_form: division by zero" in
      if Config.fail_on_exception then
	failwith msg
      else
	Log.warning_str 0 msg
    else () in
  let b_high = (abs_I (inv_I d)).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let m3 = make_stronger m3 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
  let _ = Proof.add_inv_step form_index f.form_index 
    m1 m2 m2_exp b_high m3 m3_err.proof_index in
  {
    form_index = form_index;
    v0 = mk_div const_1 f.v0;
    v1 = map (fun (e, err) -> mk_neg (mk_div e (mk_mul f.v0 f.v0)), err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* division *)
let div_form vars f1 f2 =  
  Log.report 3 "div_form";
  mul_form vars f1 (inv_form vars f2)

(* square root *)
let sqrt_form vars f = 
  Log.report 3 "sqrt_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    if Config.proof_flag then
      (x0_int +$ {low = -.m1; high = m1}) **$. 1.5
    else
      (x0_int +$ s1) **$. 1.5 in
  let b_high = 0.125 *^ (abs_I (inv_I d)).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
  let _ = Proof.add_sqrt_step form_index f.form_index 
    m1 m2 m2_exp b_high m3 m3_err.proof_index in
  let sqrt_v0 = mk_sqrt f.v0 in 
  {
    form_index = form_index;
    v0 = sqrt_v0;
    v1 = map (fun (e, err) -> mk_div e (mk_mul const_2 sqrt_v0), err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* sine *)
let sin_form vars f =
  Log.report 3 "sin_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d = 
    if Config.proof_flag then
      sin_I (x0_int +$ {low = -.m1; high = m1})
    else
      sin_I (x0_int +$ s1) in
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
  let _ = Proof.add_sin_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in
  let sin_v0 = mk_sin f.v0 in
  let cos_v0 = mk_cos f.v0 in 
  {
    form_index = form_index;
    v0 = sin_v0;
    v1 = map (fun (e, err) -> mk_mul cos_v0 e, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* cosine *)
let cos_form vars f =
  Log.report 3 "cos_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d = 
    if Config.proof_flag then
      cos_I (x0_int +$ {low = -.m1; high = m1})
    else
      cos_I (x0_int +$ s1) in
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
  let _ = Proof.add_cos_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in
  let sin_v0 = mk_sin f.v0 in
  let cos_v0 = mk_cos f.v0 in 
  {
    form_index = form_index;
    v0 = cos_v0;
    v1 = map (fun (e, err) -> mk_neg (mk_mul sin_v0 e), err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* tangent *)
(* TODO: proof support *)
let tan_form vars f =
  Log.report 3 "tan_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let xi = x0_int +$ s1 in
  let d = tan_I xi /$ pow_I_i (cos_I xi) 2 in
  let r_high = (abs_I d).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = r_high *^ m2' in
  let v1_0 = mk_mul (mk_cos f.v0) (mk_cos f.v0) in
  {
    form_index = next_form_index();
    v0 = mk_tan f.v0;
    v1 = map (fun (e, err) -> mk_div e v1_0, err) f.v1
      @ [fp_to_const m2, mk_err_var (-1) m2_exp];
  }

(* arcsine *)
(* TODO: proof support *)
let asin_form vars f =
  Log.report 3 "asin_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    xi /$ sqrt_I (pow_I_i (one_I -$ pow_I_i xi 2) 3) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(*  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_sqrt (mk_sub const_1 (mk_mul f.v0 f.v0)) in
  {
    form_index = form_index;
    v0 = mk_asin f.v0;
    v1 = map (fun (e, err) -> mk_div e v1_0, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* arccosine *)
(* TODO: proof support *)
let acos_form vars f =
  Log.report 3 "acos_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    ~-$ (xi /$ sqrt_I (pow_I_i (one_I -$ pow_I_i xi 2) 3)) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(*  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_sqrt (mk_sub const_1 (mk_mul f.v0 f.v0)) in
  {
    form_index = form_index;
    v0 = mk_acos f.v0;
    v1 = map (fun (e, err) -> mk_neg (mk_div e v1_0), err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* arctangent *)
let atan_form vars f =
  Log.report 3 "atan_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    ~-$ (xi /$ pow_I_i (pow_I_i xi 2 +$ one_I) 2) in
  let b_high = (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in
  let v1_0 = mk_add (mk_mul f.v0 f.v0) const_1 in
  {
    form_index = form_index;
    v0 = mk_atan f.v0;
    v1 = map (fun (e, err) -> mk_div e v1_0, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* exp *)
let exp_form vars f =
  Log.report 3 "exp_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    if Config.proof_flag then
      exp_I (x0_int +$ {low = -.m1; high = m1})
    else
      exp_I (x0_int +$ s1) in
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
  let _ = Proof.add_exp_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in
  let exp_v0 = mk_exp f.v0 in 
  {
    form_index = form_index;
    v0 = exp_v0;
    v1 = map (fun (e, err) -> mk_mul exp_v0 e, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* log *)
let log_form vars f =
  Log.report 3 "log_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d = 
    if Config.proof_flag then
      inv_I (pow_I_i (x0_int +$ {low = -.m1; high = m1}) 2)
    else
      inv_I (pow_I_i (x0_int +$ s1) 2) in
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
  let _ = Proof.add_log_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in
  let log_v0 = mk_log f.v0 in 
  {
    form_index = form_index;
    v0 = log_v0;
    v1 = map (fun (e, err) -> mk_div e f.v0, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* sinh *)
(* TODO: proof support *)
let sinh_form vars f =
  Log.report 3 "sinh_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    sinh_I xi in
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(*  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_cosh f.v0 in
  {
    form_index = form_index;
    v0 = mk_sinh f.v0;
    v1 = map (fun (e, err) -> mk_mul v1_0 e, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* cosh *)
(* TODO: proof support *)
let cosh_form vars f =
  Log.report 3 "cosh_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    cosh_I xi in
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(*  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_sinh f.v0 in
  {
    form_index = form_index;
    v0 = mk_cosh f.v0;
    v1 = map (fun (e, err) -> mk_mul v1_0 e, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* tanh *)
(* TODO: proof support *)
let tanh_form vars f =
  Log.report 3 "tanh_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    ~-$ (tanh_I xi /$ pow_I_i (cosh_I xi) 2) in
  let b_high = (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(*  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_mul (mk_cosh f.v0) (mk_cosh f.v0) in
  {
    form_index = form_index;
    v0 = mk_tanh f.v0;
    v1 = map (fun (e, err) -> mk_div e v1_0, err) f.v1
      @ [fp_to_const m3, m3_err];
  }


(* arsinh *)
(* TODO: proof support *)
let asinh_form vars f =
  Log.report 3 "asinh_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    ~-$ (xi /$ sqrt_I (pow_I_i (one_I +$ pow_I_i xi 2) 3)) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(*  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_sqrt (mk_add const_1 (mk_mul f.v0 f.v0)) in
  {
    form_index = form_index;
    v0 = mk_asinh f.v0;
    v1 = map (fun (e, err) -> mk_div e v1_0, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* arcosh *)
(* TODO: proof support *)
let acosh_form vars f =
  Log.report 3 "acosh_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    ~-$ (xi /$ sqrt_I (pow_I_i (pow_I_i xi 2 -$ one_I) 3)) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(*  let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_sqrt (mk_sub (mk_mul f.v0 f.v0) const_1) in
  {
    form_index = form_index;
    v0 = mk_acosh f.v0;
    v1 = map (fun (e, err) -> mk_div e v1_0, err) f.v1
      @ [fp_to_const m3, m3_err];
  }

(* artanh *)
(* TODO: proof support *)
let atanh_form vars f =
  Log.report 3 "atanh_form";
  let x0_int = estimate_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let m1 = make_stronger (abs_I s1).high in
  let d =
    let xi =
      if Config.proof_flag then
	x0_int +$ {low = -.m1; high = m1}
      else
	x0_int +$ s1 in
    xi /$ pow_I_i (one_I -$ pow_I_i xi 2) 2 in
  let b_high = (abs_I d).high in
  let b_high = make_stronger b_high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m2 = make_stronger m2 in
  let m3 = b_high *^ m2 in
  let form_index = next_form_index() in
  let m3_err = mk_err_var (-1) m2_exp in
(* let _ = Proof.add_atn_step form_index f.form_index
    m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_sub const_1 (mk_mul f.v0 f.v0) in
  {
    form_index = form_index;
    v0 = mk_atanh f.v0;
    v1 = map (fun (e, err) -> mk_div e v1_0, err) f.v1
      @ [fp_to_const m3, m3_err];
  }
    
(* absolute value *)
(* |x + e| = |x| + abs_err(t, x) * e where
   t is an upper bound of |e| and
   abs_err(t, x) = 1 if x >= t,
   abs_err(t, x) = -1 if x <= -t,
   abs_err(t, x) = [-1, 1] if -t < x < t.
   
   An interval version of abs_err can be defined as
   Abs_err(t, X) = Abs_diff(X + [-t, t]) = Abs_diff(X + sym_interval(t))
   where Abs_diff is defined as
   Abs_diff([a, b]) = 1 if a >= 0,
   Abs_diff([a, b]) = -1 if b <= 0,
   Abs_diff([a, b]) = [-1, 1] if a < 0 and b > 0.
   
   We define Abs_err directly as
   Abs_err([t1, t2], [a, b]) = 1 if a >= t2,
   Abs_err([t1, t2], [a, b]) = -1 if b <= t1,
   Abs_err([t1, t2], [a, b]) = [-1, 1] if a < t2 and b > t1.

   Abs_err(sym_interval(t), [a, b]) = Abs_diff([a, b] + sym_interval(t)).
   We prefer the explicit definition of Abs_err since it is slightly more accurate.
 *)
let abs_form vars f =
  Log.report 3 "abs_form";
  let i = next_form_index() in
  let t =
    let s, e = sum_high (abs_eval_v1 vars f.v1) in
    make_stronger (get_eps e *^ s) in
  let abs_err = mk_abs_err (mk_sym_interval (fp_to_const t)) f.v0 in
  {
    form_index = i;
    v0 = mk_abs f.v0;
    v1 = map (fun (e, err) -> mk_mul abs_err e, err) f.v1;
  }

(* maximum of two values *)
(* max(x, y) = (|x - y| + x + y) / 2.
   From this equality we get:

   max(x + e1, y + e2) = (|x - y + (e1 - e2)| + (x + y) + (e1 + e2)) / 2.

   Using the formula |x + e| = |x| + abs_err(t, x) * e 
   where t is an upper bound of |e|, we get:

   max(x + e1, y + e2) = max(x, y) + 0.5 * (abs_err(t, x - y) + 1) * e1 
                                   + 0.5 * (1 - abs_err(t, x - y)) * e2
   
   where t is an upper bound of |e1 - e2| (or |e1| + |e2| >= |e1 - e2|).
 *) 
let max_form vars f1 f2 =
  Log.report 3 "max_form";
  let i = next_form_index() in
  let t1 =
    let s, e = sum_high (abs_eval_v1 vars f1.v1) in
    make_stronger (get_eps e *^ s) in
  let t2 =
    let s, e = sum_high (abs_eval_v1 vars f2.v1) in
    make_stronger (get_eps e *^ s) in
  let x_sub_y = mk_sub f1.v0 f2.v0 in
  let t = mk_sym_interval (fp_to_const (t1 +^ t2)) in
  let err1 = mk_mul (fp_to_const 0.5) (mk_add const_1 (mk_abs_err t x_sub_y)) in
  let err2 = mk_mul (fp_to_const 0.5) (mk_sub const_1 (mk_abs_err t x_sub_y)) in
  {
    form_index = i;
    v0 = mk_max f1.v0 f2.v0;
    v1 = map (fun (e, err) -> mk_mul err1 e, err) f1.v1
         @ map (fun (e, err) -> mk_mul err2 e, err) f2.v1;
  }

(* minimum of two values *)
(* min(x, y) = -max(-x, -y) = (x + y - |x - y|) / 2,
   
   min(x + e1, y + e2) = min(x, y) + 0.5 * (1 - abs_err(t, x - y)) * e1 
                                   + 0.5 * (1 + abs_err(t, x - y)) * e2

   where t is an upper bound of |e1 - e2|.
*)
let min_form vars f1 f2 =
  Log.report 3 "min_form";
  let i = next_form_index() in
  let t1 =
    let s, e = sum_high (abs_eval_v1 vars f1.v1) in
    make_stronger (get_eps e *^ s) in
  let t2 =
    let s, e = sum_high (abs_eval_v1 vars f2.v1) in
    make_stronger (get_eps e *^ s) in
  let x_sub_y = mk_sub f1.v0 f2.v0 in
  let t = mk_sym_interval (fp_to_const (t1 +^ t2)) in
  let err1 = mk_mul (fp_to_const 0.5) (mk_sub const_1 (mk_abs_err t x_sub_y)) in
  let err2 = mk_mul (fp_to_const 0.5) (mk_add const_1 (mk_abs_err t x_sub_y)) in
  {
    form_index = i;
    v0 = mk_min f1.v0 f2.v0;
    v1 = map (fun (e, err) -> mk_mul err1 e, err) f1.v1
         @ map (fun (e, err) -> mk_mul err2 e, err) f2.v1;
  }
    
(* Builds a Taylor form *)
let build_form vars =
  let rec build e = 
    match e with
      | Const _ -> const_form e
      | Var _ -> var_form e
      | Rounding (rnd, Const c) 
	  (* when not Config.proof_flag *) -> const_rnd_form rnd (Const c)
      | Rounding (rnd, Var v) 
	  (* when not Config.proof_flag *) -> var_rnd_form rnd (Var v)
      | Rounding (rnd, Bin_op (Op_add, arg1, arg2)) 
	  when rnd.special_flag 
	       && Config.get_bool_option "fp-power2-model" 
	       && Config.get_bool_option "develop" ->
	rounded_add_form vars e rnd (build arg1) (build arg2)
      | Rounding (rnd, Bin_op (Op_sub, arg1, arg2))
	  when rnd.special_flag 
	       && Config.get_bool_option "fp-power2-model" 
	       && Config.get_bool_option "develop" ->
	rounded_sub_form vars e rnd (build arg1) (build arg2)
      | Rounding (rnd, arg) -> 
	let arg_form = build arg in
	rounded_form vars e rnd arg_form
      | U_op (op, arg) ->
	begin
	  let arg_form = build arg in
	  match op with
	    | Op_neg -> neg_form arg_form
            | Op_abs -> abs_form vars arg_form
	    | Op_inv -> inv_form vars arg_form
	    | Op_sqrt -> sqrt_form vars arg_form
	    | Op_sin -> sin_form vars arg_form
	    | Op_cos -> cos_form vars arg_form
	    | Op_tan -> tan_form vars arg_form
	    | Op_asin -> asin_form vars arg_form
	    | Op_acos -> acos_form vars arg_form
	    | Op_atan -> atan_form vars arg_form
	    | Op_exp -> exp_form vars arg_form
	    | Op_log -> log_form vars arg_form
	    | Op_sinh -> sinh_form vars arg_form
	    | Op_cosh -> cosh_form vars arg_form
	    | Op_tanh -> tanh_form vars arg_form
	    | Op_asinh -> asinh_form vars arg_form
	    | Op_acosh -> acosh_form vars arg_form
	    | Op_atanh -> atanh_form vars arg_form
	    | _ -> failwith 
	      ("build_form: unsupported unary operation " ^ op_name op)
	end
      | Bin_op (op, arg1, arg2) ->
	begin
	  let arg1_form = build arg1 and
	      arg2_form = build arg2 in
	  match op with
	    | Op_add -> add_form arg1_form arg2_form
	    | Op_sub -> sub_form arg1_form arg2_form
	    | Op_mul -> mul_form vars arg1_form arg2_form
	    | Op_div -> div_form vars arg1_form arg2_form
            | Op_max -> max_form vars arg1_form arg2_form
            | Op_min -> min_form vars arg1_form arg2_form
	    | _ -> failwith
	      ("build_form: unsupported binary operation " ^ op_name op)
	end
      | Gen_op (op, args) ->
	begin
	  let arg_forms = map build args in
	  match (op, arg_forms) with
	    | (Op_fma, [a;b;c]) -> add_form (mul_form vars a b) c
	    | _ -> failwith
	      ("build_form: unsupported general operation " ^ op_name op)
	end
  in
  fun e ->
    let _ = reset_estimate_cache() in
    let _ = reset_error_index() in
    let _ = reset_form_index() in
    let _ = reset_index_counter() in
    build e


(*
(* Builds a test expression with explicit variables representing rounding effects *)
let build_test_expr fp err_var =
  let add_rel e =
    let i = find_index fp.rounding e in
    let v = mk_var (err_var ^ string_of_int i) in
    let eps = 
      match fp.rounding with
	| Nearest -> v
	| Directed -> mk_def_mul const_2 v in
    mk_def_mul e (mk_def_add const_1 eps) in
  let rec build e = 
    match e with
      | Const c -> if is_fp_exact fp.eps c then e else add_rel e
      | Var _ -> e
      | U_op (Op_neg, flags, arg) -> U_op (Op_neg, flags, build arg)
      | U_op (op, flags, arg) ->
	let expr = U_op (op, flags, build arg) in
	if flags.op_exact then
	  expr
	else
	  add_rel expr
      | Bin_op (op, flags, arg1, arg2) ->
	let e_arg1 = build arg1 in
	let e_arg2 = build arg2 in
	let expr = Bin_op (op, flags, e_arg1, e_arg2) in
	if flags.op_exact then
	  expr
	else
	  add_rel expr
      | Gen_op (op, flags, args) ->
	let expr = Gen_op (op, flags, map build args) in
	if flags.op_exact then
	  expr
	else
	  add_rel expr
  in
  fun e ->
    let _ = reset_index_counter() in
    let result = build e in
    result, current_index()

*)
