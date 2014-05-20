(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Interval
open Num
open List
open Lib
open Expr


type rounding = Nearest | Directed

type float_parameters = {
  uncertainty_flag : bool;
  rounding : rounding;
  size : int;
  eps : float;
  (* Should be normalized by eps *)
  delta : float;
}


type taylor_form = {
  v0 : expr;
  v1 : (int * expr) list;
}

let ( +^ ) = Fpu.fadd_high and
    ( *^ ) = Fpu.fmul_high

let sum_high s = itlist (+^) s 0.0

let abs_eval vars e = 
  let v = Eval.eval_interval_expr vars e in
  (abs_I v).high

let abs_eval_v1 vars = map (fun (_, e) -> abs_eval vars e)

let fp2const f = mk_const (const_of_float f)


let simplify_form vars =
  let rec add_adjacent s =
    match s with
      | (-1, e1) :: (-1, e2) :: t ->
	let f1 = abs_eval vars e1 and
	    f2 = abs_eval vars e2 in
	add_adjacent ((-1, fp2const (f1 +^ f2)) :: t)
      | (i, e1) :: (j, e2) :: t ->
	if i = j then
	  add_adjacent ((i, mk_def_add e1 e2) :: t)
	else
	  (i, e1) :: add_adjacent ((j, e2) :: t)
      | _ -> s in
  fun f ->
    let v1 = sort (fun (i, _) (j, _) -> compare i j) f.v1 in
    {f with v1 = add_adjacent v1}

(* rounding *)
let rounded_form, find_index, expr_for_index, reset_index_counter, current_index =
  let counter = ref 0 in
  let exprs_nearest = ref [] and
      exprs_directed = ref [] in
  let get_expr_list rnd =
    match rnd with
      | Nearest -> exprs_nearest
      | Directed -> exprs_directed in
  let find_index rnd expr =
    let list = get_expr_list rnd in
    let i = assocd_eq eq_expr (-1) expr !list in
    if i > 0 then i else
      let _ = counter := !counter + 1 in
      let _ = list := (expr, !counter) :: !list in
      !counter in
  let rounded_form fp vars f =
    if fp.eps = 0.0 then
      match fp.rounding with
	| Nearest -> {
	  v0 = f.v0;
	  v1 = f.v1 @ [-1, fp2const (fp.eps *^ fp.delta)];
	}
	| Directed -> {
	  v0 = f.v0;
	  v1 = f.v1 @ [-1, fp2const (2.0 *^ fp.eps *^ fp.delta)];
	}
    else
      let i = find_index fp.rounding f.v0 in
      let m2 = (fp.eps *^ sum_high (abs_eval_v1 vars f.v1)) +^ (fp.eps *^ fp.delta) in
      let r = if Config.fp_power2_model then mk_def_floor_power2 f.v0 else f.v0 in
      match fp.rounding with
	| Nearest -> {
	  v0 = f.v0;
	  v1 = ((i, r) :: f.v1) @ [-1, fp2const m2];
	}
	| Directed -> {
	  v0 = f.v0;
	  v1 = ((i, mk_def_mul const_2 r) :: f.v1) @ [-1, fp2const (2.0 *^ m2)];
	}
  and expr_for_index i = 
    try rev_assoc i !exprs_nearest
    with Failure _ -> rev_assoc i !exprs_directed
  and reset_index_counter () = exprs_nearest := []; exprs_directed := []; counter := 0
  and current_index () = !counter
  in
  rounded_form, find_index, expr_for_index, reset_index_counter, current_index


(* For a given positive floating-point number f,
   returns a floating-point number x such that
   for any real number |r| <= f, rnd(r) = x * e with |e| <= eps
   in the given rounding mode *)
let eps_error fp f =
  if f < 0.0 then failwith "eps_error: negative number"
  else if f = 0.0 then 0.0
  else 
    let _, q = frexp f in
    match fp.rounding with
      | Nearest -> ldexp 1.0 (q - 1)
      | Directed -> ldexp 1.0 q
  
(* constant *)    
let const_form fp e = 
  match e with
    | Const c -> {
      v0 = e;
      (* Assume that constants are always rounded to the nearest value *)
      (* TODO: directed rounding? *)
      v1 = 
	begin
	  if is_fp_exact fp.eps c then [] else 
	    let bound = (abs_I c.interval_v).high in
	    let err = eps_error {fp with rounding = Nearest} bound in
	    let err_expr = Const (const_of_float err) in
	    let _ = Log.warning (Format.sprintf "Inexact constant: %s; err = %s" 
				   (print_expr_str e) 
				   (print_expr_str err_expr)) in
(*	    [find_index Nearest e, e] *)
	    [find_index Nearest e, err_expr]
	end;
    }
    | _ -> failwith ("const_form: not a constant expression: " ^ print_expr_str e)

(* variable *)
let var_form fp e =
  match e with
    | Var v ->
      let v1_uncertainty = 
	if fp.uncertainty_flag then
	  let vv = Environment.find_variable v in
	  let u = vv.Environment.uncertainty.rational_v // More_num.num_of_float fp.eps in
	  if not (u =/ Int 0) then
	    [find_index Nearest e, mk_const (const_of_num u)]
	  else []
	else [] in
      let v1_real =
	if Config.real_vars then
	  begin
	    let err_expr =
	      if Config.const_approx_real_vars then
		let bound = (abs_I (Environment.variable_interval v)).high in
		let err = eps_error fp bound in
		Const (const_of_float err) 
	      else
		match fp.rounding with
		  | Nearest -> e
		  | Directed -> mk_def_mul const_2 e in
	    let _ = Log.warning (Format.sprintf "Inexact var: %s; err = %s"
				   (print_expr_str e)
				   (print_expr_str err_expr)) in
	    [find_index fp.rounding e, err_expr]
	  end
	else [] in {
	  v0 = e;
	  v1 = v1_uncertainty @ v1_real;
	}
    | _ -> failwith ("var_form: not a variable: " ^ print_expr_str e)

(* negation *)
let neg_form f = {
  v0 = mk_def_neg f.v0;
  v1 = map (fun (i, e) -> i, mk_def_neg e) f.v1;
}

(* addition *)
let add_form f1 f2 = {
  v0 = mk_def_add f1.v0 f2.v0;
  v1 = f1.v1 @ f2.v1;
}

(* subtraction *)
let sub_form f1 f2 = {
  v0 = mk_def_sub f1.v0 f2.v0;
  v1 = f1.v1 @ map (fun (i, e) -> i, mk_def_neg e) f2.v1;
}

(* multiplication *)
let mul_form =
  let mul1 x = map (fun (i, e) -> i, mk_def_mul x e) in
  fun fp vars f1 f2 -> 
    let x1 = abs_eval_v1 vars f1.v1 and
	y1 = abs_eval_v1 vars f2.v1 in
    let m2 = fp.eps *^ (itlist (fun x s ->
      sum_high (map (fun y -> x *^ y) y1) +^ s) x1 0.0) in 
    {
      v0 = mk_def_mul f1.v0 f2.v0;
      v1 = mul1 f1.v0 f2.v1 @ mul1 f2.v0 f1.v1 @ [-1, fp2const m2];
    }

(* reciprocal *)
let inv_form fp vars f = 
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let xi = {low = -. fp.eps; high = fp.eps} in
  let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
  let d = pow_I_i (x0_int +$ s1) 3 in
  let r_high = (abs_I (inv_I d)).high in
  let m2 = fp.eps *^ (r_high *^ itlist (fun a s ->
    sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0) in
  {
    v0 = mk_def_div const_1 f.v0;
    v1 = map (fun (i, e) -> i, mk_def_neg (mk_def_div e (mk_def_mul f.v0 f.v0))) f.v1
      @ [-1, fp2const m2];
  }

(* division *)
let div_form fp vars f1 f2 =  
  mul_form fp vars f1 (inv_form fp vars f2);;


(* square root *)
let sqrt_form fp vars f = 
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let xi = {low = -. fp.eps; high = fp.eps} in
  let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
  let d = (x0_int +$ s1) **$. 1.5 in
  let r_high = (abs_I (inv_I d)).high in
  let m2 = fp.eps *^ (0.125 *^ r_high *^ itlist (fun a s ->
    sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0) in
  let sqrt_v0 = mk_def_sqrt f.v0 in 
  {
    v0 = sqrt_v0;
    v1 = map (fun (i, e) -> i, mk_def_div e (mk_def_mul const_2 sqrt_v0)) f.v1
      @ [-1, fp2const m2];
  }

(* sine *)
let sin_form fp vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let xi = {low = -. fp.eps; high = fp.eps} in
  let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
  let d = sin_I (x0_int +$ s1) in
  let r_high = (abs_I d).high in
  let m2 = fp.eps *^ (0.5 *^ r_high *^ itlist (fun a s ->
    sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0) in
  let sin_v0 = mk_def_sin f.v0 in
  let cos_v0 = mk_def_cos f.v0 in 
  {
    v0 = sin_v0;
    v1 = map (fun (i, e) -> i, mk_def_mul cos_v0 e) f.v1
      @ [-1, fp2const m2];
  }

(* cosine *)
let cos_form fp vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let xi = {low = -. fp.eps; high = fp.eps} in
  let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
  let d = cos_I (x0_int +$ s1) in
  let r_high = (abs_I d).high in
  let m2 = fp.eps *^ (0.5 *^ r_high *^ itlist (fun a s ->
    sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0) in
  let sin_v0 = mk_def_sin f.v0 in
  let cos_v0 = mk_def_cos f.v0 in 
  {
    v0 = cos_v0;
    v1 = map (fun (i, e) -> i, mk_def_neg (mk_def_mul sin_v0 e)) f.v1
      @ [-1, fp2const m2];
  }

(* exp *)
let exp_form fp vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let xi = {low = -. fp.eps; high = fp.eps} in
  let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
  let d = exp_I (x0_int +$ s1) in
  let r_high = (abs_I d).high in
  let m2 = fp.eps *^ (0.5 *^ r_high *^ itlist (fun a s ->
    sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0) in
  let exp_v0 = mk_def_exp f.v0 in 
  {
    v0 = exp_v0;
    v1 = map (fun (i, e) -> i, mk_def_mul exp_v0 e) f.v1
      @ [-1, fp2const m2];
  }

(* log *)
let log_form fp vars f =
  let x0_int = Eval.eval_interval_expr vars f.v0 in
  let x1 = abs_eval_v1 vars f.v1 in
  let xi = {low = -. fp.eps; high = fp.eps} in
  let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
  let d = inv_I (pow_I_i (x0_int +$ s1) 2) in
  let r_high = (abs_I d).high in
  let m2 = fp.eps *^ (0.5 *^ r_high *^ itlist (fun a s ->
    sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0) in
  let log_v0 = mk_def_log f.v0 in 
  {
    v0 = log_v0;
    v1 = map (fun (i, e) -> i, mk_def_div e f.v0) f.v1
      @ [-1, fp2const m2];
  }


let is_power_of_2_or_0 e =
  match e with
    | Const c -> 
      let n = c.rational_v in
      n =/ Int 0 || More_num.is_power_of_two n
    | _ -> false

(* Builds a Taylor form *)
let build_form fp vars =
  let rec build e = 
    match e with
      | Const _ -> const_form fp e
      | Var _ -> var_form fp e
      | U_op (Op_neg, _, arg) -> neg_form (build arg)
      | U_op (op, flags, arg) ->
	begin
	  let arg_form = build arg in
	  let form =
	    match op with
	      | Op_inv -> inv_form fp vars arg_form
	      | Op_sqrt -> sqrt_form fp vars arg_form
	      | Op_sin -> sin_form fp vars arg_form
	      | Op_cos -> cos_form fp vars arg_form
	      | Op_exp -> exp_form fp vars arg_form
	      | Op_log -> log_form fp vars arg_form
	      | _ -> failwith 
		("build_form: unsupported unary operation " ^ op_name op flags) in
	  if flags.op_exact then
	    form
	  else
	    rounded_form fp vars form
	end
      | Bin_op (op, flags, arg1, arg2) ->
	begin
	  let arg1_form = build arg1 and
	      arg2_form = build arg2 in
	  let form, fp' =
	    match op with
	      (* subnormal delta is zero for addition and subtraction *)
	      | Op_add -> add_form arg1_form arg2_form, {fp with delta = 0.0}
	      | Op_sub -> sub_form arg1_form arg2_form, {fp with delta = 0.0}
	      | Op_mul -> 
		let r = mul_form fp vars arg1_form arg2_form in
		if is_power_of_2_or_0 arg1 || is_power_of_2_or_0 arg2 then
		  r, {fp with eps = 0.0; delta = 0.0}
		else
		  r, fp
	      | Op_div -> 
		let r = div_form fp vars arg1_form arg2_form in
		if is_power_of_2_or_0 arg2 then
		  r, {fp with eps = 0.0}
		else
		  r, fp
	      | _ -> failwith
		("build_form: unsupported binary operation " ^ op_name op flags) in
	  if flags.op_exact then
	    form
	  else
	    rounded_form fp' vars form
	end
      | Gen_op (op, flags, args) ->
	begin
	  let arg_forms = map build args in
	  let form =
	    match (op, arg_forms) with
	      | (Op_fma, [a;b;c]) -> add_form (mul_form fp vars a b) c
	      | _ -> failwith
		("build_form: unsupported general operation " ^ op_name op flags) in
	  if flags.op_exact then
	    form
	  else
	    rounded_form fp vars form
	end
  in
  fun e ->
    let _ = reset_index_counter() in
    build e


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

