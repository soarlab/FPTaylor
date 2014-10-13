open List
open Num
open Rounding
open Expr
open Interval
module Env = Environment

exception Exceptional_operation of expr * string


let is_power_of_2_or_0 e =
  match e with
    | Const c -> 
      let n = c.rational_v in
      n =/ Int 0 || More_num.is_power_of_two n
    | _ -> false


let rec get_type e =
  match e with
    | Const _ -> real_type
    | Var name -> Env.get_var_type name
    | U_op (op, arg) ->
      begin
	let arg_type = get_type arg in
	match op with
	  | Op_neg -> arg_type
	  | _ -> real_type
      end
    | Bin_op (Op_mul, arg1, arg2) ->
      if is_power_of_2_or_0 arg1 then 
	get_type arg2
      else if is_power_of_2_or_0 arg2 then
	get_type arg1
      else
	real_type
    | Bin_op (op, arg1, arg2) -> real_type
    | Gen_op _ -> real_type
    | Rounding (rnd, _) -> rnd.fp_type


let simplify_rounding =
  let rec simplify e =
    match e with
      | Const _ -> e
      | Var _ -> e
      | U_op (op, arg) ->
	let e1 = simplify arg in
	U_op (op, e1)
      | Bin_op (op, arg1, arg2) ->
	let e1 = simplify arg1 and
	    e2 = simplify arg2 in
	Bin_op (op, e1, e2)
      | Gen_op (op, args) ->
	let es = map simplify args in
	Gen_op (op, es)
      | Rounding (rnd, arg) ->
	begin
	  let arg = simplify arg in
	  let ty = get_type arg in
	  if rnd.fp_type.bits = 0 then
	    (* No rounding *)
	    arg
	  else if is_subtype ty rnd.fp_type then
	    (* Rounding is exact *)
	    arg
	  else
	    match arg with
	      (* Const *)
	      | Const c ->
		if is_fp_exact (get_eps rnd.eps_exp) c then 
		  arg 
		else
		  Rounding (rnd, arg)
	      (* Plus or minus *)
	      | Bin_op (Op_add, e1, e2) | Bin_op (Op_sub, e1, e2) ->
		if (is_subtype (get_type e1) rnd.fp_type && 
		      is_subtype (get_type e2) rnd.fp_type &&
		      not Config.proof_flag) then
		(* Delta = 0 *)
		  Rounding ({rnd with delta_exp = 0}, arg)
		else
		  Rounding (rnd, arg)
	      (* Multiplication *)
	      | Bin_op (Op_mul, e1, e2) when 
		  (is_power_of_2_or_0 e1 && is_subtype (get_type e2) rnd.fp_type) 
		  || (is_power_of_2_or_0 e2 && is_subtype (get_type e1) rnd.fp_type) ->
		arg
	      (* Division *)
	      | Bin_op (Op_div, e1, e2) when
		  is_power_of_2_or_0 e2 && 
		    is_subtype (get_type e1) rnd.fp_type &&
		    not Config.proof_flag ->
		(* Eps = 0 *)
		Rounding ({rnd with eps_exp = 0}, arg)
	      | _ -> Rounding (rnd, arg)
	end 
  in
  simplify
  
		
(* A very conservative interval rounding *)
let rnd_I rnd x =
  let e = get_eps rnd.eps_exp and
      d = get_eps rnd.delta_exp in
  let ( *^ ) = Fpu.fmul_high and
      ( +^ ) = Fpu.fadd_high and
      ( -^ ) = Fpu.fsub_low in
  let extra v = rnd.coefficient *^ ((e *^ abs_float v) +^ d) in
  let over v = 
    if abs_float v > rnd.max_val then 
      (if v < 0.0 then neg_infinity else infinity)
    else v in
  {
    low = over (x.low -^ extra x.low);
    high = over (x.high +^ extra x.high);
  }

let check_float v =
  match (classify_float v) with
    | FP_infinite -> "Overflow"
    | FP_nan -> "NaN"
    | _ -> ""

let check_interval x =
  let c1 = check_float x.high in
  if c1 = "" then check_float x.low else c1
    
(* A conservative safety check procedure *)
let check_expr vars =
  let rec eval e =
    let r = match e with
      | Const c -> c.interval_v
      | Var v -> vars v
      | Rounding (rnd, e1) ->
	let r1 = eval e1 in
	if rnd.fp_type.bits = 0 then
	  r1
	else
	  rnd_I rnd r1
      | U_op (op, arg) ->
	begin
	  let x = eval arg in
	  match op with
	    | Op_neg -> ~-$ x
	    | Op_abs -> abs_I x
	    | Op_inv -> 
	      if compare_I_f x 0.0 = 0 then
		raise (Exceptional_operation (e, "Division by zero"))
	      else
		inv_I x
	    | Op_sqrt -> 
	      if x.low < 0.0 then
		raise (Exceptional_operation (e, "Sqrt of negative number"))
	      else
		sqrt_I x
	  | Op_sin -> sin_I x
	  | Op_cos -> cos_I x
	  | Op_tan -> tan_I x
	  | Op_exp -> exp_I x
	  | Op_log -> 
	    if x.low <= 0.0 then
	      raise (Exceptional_operation (e, "Log of non-positive number"))
	    else
	      log_I x
	  | Op_floor_power2 -> Eval.floor_power2_I x
	  | Op_sym_interval -> Eval.sym_interval_I x
	  | _ -> failwith ("check_expr: Unsupported unary operation: " 
			   ^ op_name op)
	end
      | Bin_op (op, arg1, arg2) ->
	begin
	  let x1 = eval arg1 in
	  match op with
	    | Op_add -> x1 +$ eval arg2
	    | Op_sub -> x1 -$ eval arg2
	    | Op_mul ->
	    (* A temporary solution to increase accuracy *)
	      if eq_expr arg1 arg2 then
		pow_I_i x1 2
	      else
		x1 *$ eval arg2
	    | Op_div -> 
	      let x2 = eval arg2 in
	      if compare_I_f x2 0.0 = 0 then
		raise (Exceptional_operation (e, "Division by zero"))
	      else
		x1 /$ x2
	    | Op_nat_pow -> x1 **$. (Eval.eval_float_const_expr arg2)
	    | _ -> failwith ("eval_interval_expr: Unsupported binary operation: " 
			     ^ op_name op)
	end
      | Gen_op (op, args) ->
	begin
	  let xs = map eval args in
	  match (op, xs) with
	    | (Op_fma, [a;b;c]) -> (a *$ b) +$ c
	    | _ -> failwith ("eval_interval_expr: Unsupported general operation: " 
			     ^ op_name op)
	end
    in
    let c = check_interval r in
    if c <> "" then
      raise (Exceptional_operation (e, c))
    else
      r in
  eval

