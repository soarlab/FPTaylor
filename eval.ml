open Interval
open Num
open Expr
open List

(* For a given positive floating-point number f,
   returns the largest floating-point number 2^n such that
   2^n <= f.
*)
let floor_power2 =
  let p2 f =
    let _, q = frexp f in
    ldexp 1.0 (q - 1) in
  fun f ->
    match (classify_float f) with
      | FP_zero -> f
      | FP_infinite -> f
      | FP_nan -> f
      | FP_normal | FP_subnormal -> 
	if f < 0.0 then -.p2 (-.f) else p2 f

let floor_power2_I x = {
  low = floor_power2 x.low;
  high = floor_power2 x.high
}

let sym_interval_I x = 
  let f = (abs_I x).high in {
    low = -.f;
    high = f;
  }

(* Computes a floating-point value of an expression *)
(* vars : string -> float is a function which associates 
   floating-point values with variable names *)
let eval_float_expr vars =
  let rec eval = function
    | Const c -> c.float_v
    | Var v -> vars v
    | U_op (op, f, arg) ->
      begin
	let x = eval arg in
	match op with
	  | Op_neg -> -.x
	  | Op_abs -> abs_float x
	  | Op_inv -> 1.0 /. x
	  | Op_sqrt -> sqrt x
	  | Op_sin -> sin x
	  | Op_cos -> cos x
	  | Op_tan -> tan x
	  | Op_exp -> exp x
	  | Op_log -> log x
	  | Op_floor_power2 -> floor_power2 x
	  | Op_sym_interval -> 0.0
	  | _ -> failwith ("eval_float_expr: Unsupported unary operation: " 
			   ^ op_name op f)
      end
    | Bin_op (op, f, arg1, arg2) ->
      begin
	let x1 = eval arg1 and
	    x2 = eval arg2 in
	match op with
	  | Op_add -> x1 +. x2
	  | Op_sub -> x1 -. x2
	  | Op_mul -> x1 *. x2
	  | Op_div -> x1 /. x2
	  | Op_nat_pow -> x1 ** x2
	  | _ -> failwith ("eval_float_expr: Unsupported binary operation: " 
			   ^ op_name op f)
      end
    | Gen_op (op, f, args) ->
      begin
	let xs = map eval args in
	match (op, xs) with
	  | (Op_fma, [a;b;c]) -> a *. b +. c
	  | _ -> failwith ("eval_float_expr: Unsupported general operation: " 
			   ^ op_name op f)
      end
  in
  eval

let eval_float_const_expr =
  eval_float_expr (fun v -> failwith ("eval_float_const_expr: Var " ^ v))

(* Computes a rational value of an expression *)
(* vars : string -> num is a function which associates 
   rational values with variable names *)
let eval_num_expr vars =
  let one = Int 1 in
  let rec eval = function
    | Const c -> c.rational_v
    | Var v -> vars v
    | U_op (op, f, arg) ->
      begin
	let x = eval arg in
	match op with
	  | Op_neg -> minus_num x
	  | Op_abs -> abs_num x
	  | Op_inv -> one // x
	  | _ -> failwith ("eval_num_expr: Unsupported unary operation: " 
			   ^ op_name op f)
      end
    | Bin_op (op, f, arg1, arg2) ->
      begin
	let x1 = eval arg1 and
	    x2 = eval arg2 in
	match op with
	  | Op_add -> x1 +/ x2
	  | Op_sub -> x1 -/ x2
	  | Op_mul -> x1 */ x2
	  | Op_div -> x1 // x2
	  | Op_nat_pow -> x1 **/ x2
	  | _ -> failwith ("eval_num_expr: Unsupported binary operation: " 
			   ^ op_name op f)
      end
    | Gen_op (op, f, args) ->
      begin
	let xs = map eval args in
	match (op, xs) with
	  | (Op_fma, [a;b;c]) -> (a */ b) +/ c
	  | _ -> failwith ("eval_num_expr: Unsupported general operation: " 
			   ^ op_name op f)
      end
  in
  eval

let eval_num_const_expr =
  eval_num_expr (fun v -> failwith ("eval_num_const_expr: Var " ^ v))

(* Computes an interval value of an expression *)
(* vars : string -> interval is a function which associates 
   inteval values with variable names *)
let eval_interval_expr vars =
  let rec eval = function
    | Const c -> c.interval_v
    | Var v -> vars v
    | U_op (op, f, arg) ->
      begin
	let x = eval arg in
	match op with
	  | Op_neg -> ~-$ x
	  | Op_abs -> abs_I x
	  | Op_inv -> inv_I x
	  | Op_sqrt -> sqrt_I x
	  | Op_sin -> sin_I x
	  | Op_cos -> cos_I x
	  | Op_tan -> tan_I x
	  | Op_exp -> exp_I x
	  | Op_log -> log_I x
	  | Op_floor_power2 -> floor_power2_I x
	  | Op_sym_interval -> sym_interval_I x
	  | _ -> failwith ("eval_interval_expr: Unsupported unary operation: " 
			   ^ op_name op f)
      end
    | Bin_op (op, f, arg1, arg2) ->
      begin
	let x1 = eval arg1 in
	match op with
	  | Op_add -> x1 +$ eval arg2
	  | Op_sub -> x1 -$ eval arg2
	  | Op_mul ->
	    (* A temporary solution to deal with the Jet benchmark *)
	    if eq_expr arg1 arg2 then
	      pow_I_i x1 2
	    else
	      x1 *$ eval arg2
	  | Op_div -> x1 /$ eval arg2
	  | Op_nat_pow -> x1 **$. (eval_float_const_expr arg2)
	  | _ -> failwith ("eval_interval_expr: Unsupported binary operation: " 
			   ^ op_name op f)
      end
    | Gen_op (op, f, args) ->
      begin
	let xs = map eval args in
	match (op, xs) with
	  | (Op_fma, [a;b;c]) -> (a *$ b) +$ c
	  | _ -> failwith ("eval_interval_expr: Unsupported general operation: " 
			   ^ op_name op f)
      end
  in
  eval

let eval_interval_const_expr =
  eval_interval_expr (fun v -> failwith ("eval_interval_const_expr: Var " ^ v))


let eval_const_expr e = {
  rational_v = eval_num_const_expr e;
  float_v = eval_float_const_expr e;
  interval_v = eval_interval_const_expr e;
}
