(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Interval
open Num
open List
open Lib
open Rounding
open Expr

(* Describes an error variable *)
type error_info = {
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

let reset_form_index, next_form_index =
  let index = ref 0 in
  let reset () = index := 0 in
  let next () =
    let i = !index in
    let _ = incr index in
    i in
  reset, next

let mk_err_var index exp = {
  index = index;
  exp = exp;
}

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
  let v = Eval.eval_interval_expr vars ex in
  (abs_I v).high

let abs_eval_v1 vars = map (fun (ex, err) -> abs_eval vars ex, err.exp)

let fp_to_const f = mk_const (const_of_float f)

let simplify_form vars =
  let rec add_adjacent s =
    match s with
      | (ex1, err1) :: (ex2, err2) :: t when err1.index = -1 && err2.index = -1 ->
	let f1, exp1 = abs_eval vars ex1, err1.exp and
	    f2, exp2 = abs_eval vars ex2, err2.exp in
	let f, exp = add2 (f1, exp1) (f2, exp2) in
	add_adjacent ((fp_to_const f, mk_err_var (-1) exp) :: t)
      | (e1, err1) :: (e2, err2) :: t ->
	if err1.index = err2.index then
	  (* we must have err1.exp = err2.exp here *)
	  let _ = assert(err1.exp = err2.exp) in
	  add_adjacent ((mk_add e1 e2, err1) :: t)
	else
	  (e1, err1) :: add_adjacent ((e2, err2) :: t)
      | _ -> s in
  fun f ->
    let v1 = sort (fun (_, err1) (_, err2) -> compare err1.index err2.index) f.v1 in
    {f with v1 = add_adjacent v1}

let find_index, expr_for_index, reset_index_counter, current_index =
  let counter = ref 0 in
  let exprs = ref [] in
  let find_index expr =
    let i = assocd_eq eq_expr (-1) expr !exprs in
    if i > 0 then i else
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
  match e with
    | Const c -> 
      let i = next_form_index() in
      let _ = Proof.add_const_step i c.rational_v in {
	form_index = i;
	v0 = e;
	v1 = []
      }
    | _ -> failwith ("const_form: not a constant" ^ print_expr_str e)

(* constant with rounding *)
let const_rnd_form rnd e =
  match e with
    | Const c -> {
      form_index = next_form_index();
      v0 = e;
      v1 =
	begin
	  if is_fp_exact (get_eps rnd.eps_exp) c then [] else
	    let bound = (abs_I c.interval_v).high in
	    let err = rnd.coefficient *^ floor_power2 bound in
	    let err_expr = fp_to_const err in
	    let _ = Log.warning (Format.sprintf "Inexact constant: %s; err = %s" 
				   (print_expr_str e) 
				   (print_expr_str err_expr)) in
	    (* TODO: subnormal constants (a very rare case) *)
	    [err_expr, mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp]
	end;
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
  match e with
    | Var v -> 
      let i = next_form_index() in
      let _ = Proof.add_var_step i v in {
	form_index = i;
	v0 = e;
      (* FIXME: what is the right value of eps_exp here? *)
	v1 = if Config.uncertainty then get_var_uncertainty (-53) v else [];
      }
    | _ -> failwith ("var_form: not a variable" ^ print_expr_str e)

(* variable with rounding *)
let var_rnd_form rnd e =
  match e with
    | Var v -> 
      let v1_uncertainty = 
	if Config.uncertainty then get_var_uncertainty rnd.eps_exp v else [] in
      let v1_rnd =
	let err_expr0 = 
	  if Config.const_approx_real_vars then
	    let bound = (abs_I (Environment.variable_interval v)).high in
	    let err = floor_power2 bound in
	    fp_to_const err
	  else if Config.fp_power2_model then
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
  if rnd.eps_exp = 0 then {
    form_index = next_form_index();
    v0 = f.v0;
    v1 = f.v1 @ [fp_to_const rnd.coefficient, mk_err_var (-1) rnd.delta_exp]
  }
  else
    let i = find_index original_expr in
    let s1', exp1 = sum_high (abs_eval_v1 vars f.v1) in
    let s1 = get_eps exp1 *^ s1' in
    let r', m2' =
      if Config.fp_power2_model then
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
    let _ = Proof.add_rnd_step form_index rnd.fp_type.bits f.form_index m2 in
    {
      form_index = form_index;
      v0 = f.v0;
      v1 = ((r, mk_err_var i rnd.eps_exp) :: f.v1) @ 
	[fp_to_const m2, mk_err_var (-1) rnd.eps_exp];
    }

(* negation *)
let neg_form f = 
  let i = next_form_index() in
  let _ = Proof.add_neg_step i f.form_index in {
    form_index = i;
    v0 = mk_neg f.v0;
    v1 = map (fun (e, err) -> mk_neg e, err) f.v1;
  }

(* addition *)
let add_form f1 f2 = 
  let i = next_form_index() in
  let _ = Proof.add_add_step i f1.form_index f2.form_index in {
    form_index = i;
    v0 = mk_add f1.v0 f2.v0;
    v1 = f1.v1 @ f2.v1;
  }

(* subtraction *)
let sub_form f1 f2 =
  let i = next_form_index() in
  let _ = Proof.add_sub_step i f1.form_index f2.form_index in {
    form_index = i;
    v0 = mk_sub f1.v0 f2.v0;
    v1 = f1.v1 @ map (fun (e, err) -> mk_neg e, err) f2.v1;
  }

(* multiplication *)
let mul_form =
  let mul1 x = map (fun (e, err) -> mk_mul x e, err) in
  fun vars f1 f2 -> 
    let x1 = abs_eval_v1 vars f1.v1 and
	y1 = abs_eval_v1 vars f2.v1 in
    let m2, m2_exp = sum2_high x1 y1 in
    let m2 = make_stronger m2 in
    let form_index = next_form_index() in
    let _ = Proof.add_mul_step form_index f1.form_index f2.form_index m2 in
    {
      form_index = form_index;
      v0 = mk_mul f1.v0 f2.v0;
      v1 = mul1 f1.v0 f2.v1 @ mul1 f2.v0 f1.v1 @ [fp_to_const m2, mk_err_var (-1) m2_exp];
    }

(* reciprocal *)
let inv_form vars f = 
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let d = pow_I_i (x0_int +$ s1) 3 in
  let r_high = (abs_I (inv_I d)).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = r_high *^ m2' in
  let m2 = make_stronger m2 in
  let form_index = next_form_index() in
  let _ = Proof.add_inv_step form_index f.form_index m2 in
  {
    form_index = form_index;
    v0 = mk_div const_1 f.v0;
    v1 = map (fun (e, err) -> mk_neg (mk_div e (mk_mul f.v0 f.v0)), err) f.v1
      @ [fp_to_const m2, mk_err_var (-1) m2_exp];
  }

(* division *)
let div_form vars f1 f2 =  
  mul_form vars f1 (inv_form vars f2);;

(* square root *)
let sqrt_form vars f = 
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let d = (x0_int +$ s1) **$. 1.5 in
  let r_high = (abs_I (inv_I d)).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = 0.125 *^ r_high *^ m2' in
  let sqrt_v0 = mk_sqrt f.v0 in 
  let m2 = make_stronger m2 in
  let form_index = next_form_index() in
  let _ = Proof.add_sqrt_step form_index f.form_index m2 in
  {
    form_index = next_form_index();
    v0 = sqrt_v0;
    v1 = map (fun (e, err) -> mk_div e (mk_mul const_2 sqrt_v0), err) f.v1
      @ [fp_to_const m2, mk_err_var (-1) m2_exp];
  }

(* sine *)
let sin_form vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let d = sin_I (x0_int +$ s1) in
  let r_high = (abs_I d).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = 0.5 *^ r_high *^ m2' in
  let sin_v0 = mk_sin f.v0 in
  let cos_v0 = mk_cos f.v0 in 
  {
    form_index = next_form_index();
    v0 = sin_v0;
    v1 = map (fun (e, err) -> mk_mul cos_v0 e, err) f.v1
      @ [fp_to_const m2, mk_err_var (-1) m2_exp];
  }

(* cosine *)
let cos_form vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let d = cos_I (x0_int +$ s1) in
  let r_high = (abs_I d).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = 0.5 *^ r_high *^ m2' in
  let sin_v0 = mk_sin f.v0 in
  let cos_v0 = mk_cos f.v0 in 
  {
    form_index = next_form_index();
    v0 = cos_v0;
    v1 = map (fun (e, err) -> mk_neg (mk_mul sin_v0 e), err) f.v1
      @ [fp_to_const m2, mk_err_var (-1) m2_exp];
  }

(* exp *)
let exp_form vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let d = exp_I (x0_int +$ s1) in
  let r_high = (abs_I d).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = 0.5 *^ r_high *^ m2' in
  let exp_v0 = mk_exp f.v0 in 
  {
    form_index = next_form_index();
    v0 = exp_v0;
    v1 = map (fun (e, err) -> mk_mul exp_v0 e, err) f.v1
      @ [fp_to_const m2, mk_err_var (-1) m2_exp];
  }

(* log *)
let log_form vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let s1 = itlist (fun (x,x_exp) s -> 
    let eps = get_eps x_exp in
    let xi = {low = -. eps; high = eps} in
    (xi *$. x) +$ s) x1 zero_I in
  let d = inv_I (pow_I_i (x0_int +$ s1) 2) in
  let r_high = (abs_I d).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = 0.5 *^ r_high *^ m2' in
  let log_v0 = mk_log f.v0 in 
  {
    form_index = next_form_index();
    v0 = log_v0;
    v1 = map (fun (e, err) -> mk_div e f.v0, err) f.v1
      @ [fp_to_const m2, mk_err_var (-1) m2_exp];
  }

(* Builds a Taylor form *)
let build_form vars =
  let rec build e = 
    match e with
      | Const _ -> const_form e
      | Var _ -> var_form e
      | Rounding (rnd, Const c) 
	  when not Config.proof_flag -> const_rnd_form rnd (Const c)
      | Rounding (rnd, Var v) 
	  when not Config.proof_flag -> var_rnd_form rnd (Var v)
      | Rounding (rnd, arg) -> 
	let arg_form = build arg in
	rounded_form vars e rnd arg_form
      | U_op (op, arg) ->
	begin
	  let arg_form = build arg in
	  match op with
	    | Op_neg -> neg_form arg_form
	    | Op_inv -> inv_form vars arg_form
	    | Op_sqrt -> sqrt_form vars arg_form
	    | Op_sin -> sin_form vars arg_form
	    | Op_cos -> cos_form vars arg_form
	    | Op_exp -> exp_form vars arg_form
	    | Op_log -> log_form vars arg_form
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
