(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Interval
open Num
open List
open Lib
open Expr

type taylor_form = {
  v0 : expr;
  v1 : (int * expr) list;
  m2 : float;
}

type float_parameters = {
  uncertainty_flag : bool;
  eps : float;
  (* Should be normalized by eps^2 *)
  delta : float;
}

let simplify_form =
  let rec add_adjacent s =
    match s with
      | (i, e1) :: (j, e2) :: t ->
	if i = j then
	  add_adjacent ((i, mk_def_add e1 e2) :: t)
	else
	  (i, e1) :: add_adjacent ((j, e2) :: t)
      | _ -> s in
  fun f ->
    let v1 = sort (fun (i, _) (j, _) -> compare i j) f.v1 in
    {f with v1 = add_adjacent v1}

let ( +^ ) = Fpu.fadd_high and
    ( *^ ) = Fpu.fmul_high

let sum_high s = itlist (+^) s 0.0

let abs_eval vars e = 
  let v = Eval.eval_interval_expr vars e in
  (abs_I v).high

let abs_eval_v1 vars = map (fun (_, e) -> abs_eval vars e)

(* rounding *)
let rounded_form, find_index, expr_for_index, reset_index_counter =
  let counter = ref 0 in
  let exprs = ref [] in
  let find_index ex =
    let i = assocd_eq eq_expr (-1) ex !exprs in
    if i > 0 then i else
      let _ = counter := !counter + 1 in
      let _ = exprs := (ex, !counter) :: !exprs in
      !counter in
  let rounded_form fp vars f =
    let i = find_index f.v0 in {
      v0 = f.v0;
      v1 = (i, f.v0) :: f.v1;
      m2 = f.m2 +^ (fp.eps *^ f.m2) +^ sum_high (abs_eval_v1 vars f.v1) +^ fp.delta;
    }
  and expr_for_index i = rev_assoc i !exprs
  and reset_index_counter () = exprs := []; counter := 0 
  in
  rounded_form, find_index, expr_for_index, reset_index_counter
  
(* constant *)    
let const_form fp e = 
  match e with
    | Const c -> {
      v0 = e;
      v1 = if is_fp_exact fp.eps c then [] else [find_index e, e];
      m2 = 0.0;
    }
    | _ -> failwith ("const_form: not a constant expression: " ^ print_expr_str e)

(* variable *)
let var_form fp e =
  match e with
    | Var v -> {
      v0 = e;
      v1 = 
	if fp.uncertainty_flag then
	  let vv = Environment.find_variable v in
	  let u = vv.Environment.uncertainty.rational_v // More_num.num_of_float fp.eps in
	  if not (u =/ Int 0) then
	    [find_index e, mk_const (const_of_num u)]
	  else []
	else [];
      m2 = 0.0
    }
    | _ -> failwith ("var_form: not a variable: " ^ print_expr_str e)

(* negation *)
let neg_form f = {
  v0 = mk_def_neg f.v0;
  v1 = map (fun (i, e) -> i, mk_def_neg e) f.v1;
  m2 = f.m2;
}

(* addition *)
let add_form f1 f2 = {
  v0 = mk_def_add f1.v0 f2.v0;
  v1 = f1.v1 @ f2.v1;
  m2 = f1.m2 +^ f2.m2;
}

(* subtraction *)
let sub_form f1 f2 = {
  v0 = mk_def_sub f1.v0 f2.v0;
  v1 = f1.v1 @ map (fun (i, e) -> i, mk_def_neg e) f2.v1;
  m2 = f1.m2 +^ f2.m2;
}

(* multiplication *)
let mul_form =
  let mul1 x = map (fun (i, e) -> i, mk_def_mul x e) in
  fun fp vars f1 f2 -> {
    v0 = mk_def_mul f1.v0 f2.v0;
    v1 = mul1 f1.v0 f2.v1 @ mul1 f2.v0 f1.v1;
    m2 =
      let x0 = abs_eval vars f1.v0 and
	  y0 = abs_eval vars f2.v0 and
	  x1 = abs_eval_v1 vars f1.v1 and
	  y1 = abs_eval_v1 vars f2.v1 and
	  x2 = f1.m2 and
	  y2 = f2.m2 in
      let a = (y0 *^ x2) +^ (x0 *^ y2) +^ ((x2 *^ fp.eps) *^ (y2 *^ fp.eps)) in
      let b = (sum_high x1 *^ y2 *^ fp.eps) +^ (sum_high y1 *^ x2 *^ fp.eps) in
      let c = itlist (fun x s ->
	sum_high (map (fun y -> x *^ y) y1) +^ s) x1 0.0 in
      let r = a +^ b +^ c in
      r;
  }

(* reciprocal *)
let inv_form fp vars f = {
  v0 = mk_def_div const_1 f.v0;
  v1 = map (fun (i, e) -> i, mk_def_neg (mk_def_div e (mk_def_mul f.v0 f.v0))) f.v1;
  m2 = 
    let x0_int = Eval.eval_interval_expr vars f.v0 in
    let x1 = (f.m2 *^ fp.eps) :: (abs_eval_v1 vars f.v1) in
    let xi = {low = -. fp.eps; high = fp.eps} in
    let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
    let d = pow_I_i (x0_int +$ s1) 3 in
    let r_high = (abs_I (inv_I d)).high in
    let s2 = r_high *^ itlist (fun a s ->
      sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0 in
    let s0 = (f.m2 /.$ (pow_I_i x0_int 2)).high in
    s0 +^ s2;
}

(* division *)
let div_form fp vars f1 f2 =  
  mul_form fp vars f1 (inv_form fp vars f2);;

(* square root *)
let sqrt_form fp vars f = 
  let sqrt_v0 = mk_def_sqrt f.v0 in {
    v0 = sqrt_v0;
    v1 = map (fun (i, e) -> i, mk_def_div e (mk_def_mul const_2 sqrt_v0)) f.v1;
    m2 = 
      let x0_int = Eval.eval_interval_expr vars f.v0 in
      let x1 = (f.m2 *^ fp.eps) :: (abs_eval_v1 vars f.v1) in
      let xi = {low = -. fp.eps; high = fp.eps} in
      let s1 = itlist (fun a s -> (xi *$. a) +$ s) x1 zero_I in
      let d = (x0_int +$ s1) **$. 1.5 in
      let r_high = (abs_I (inv_I d)).high in
      let s2 = 0.125 *^ r_high *^ itlist (fun a s ->
	sum_high (map (fun b -> a *^ b) x1) +^ s) x1 0.0 in
      let s0 = 0.5 *^ (f.m2 /.$ sqrt_I x0_int).high in
      s0 +^ s2;
  }

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
	      | Op_mul -> mul_form fp vars arg1_form arg2_form, fp
	      | Op_div -> div_form fp vars arg1_form arg2_form, fp
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
