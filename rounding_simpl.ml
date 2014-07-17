open List
open Num
open Rounding
open Expr
module Env = Environment


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
	  if is_subtype ty rnd.fp_type then
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
		if is_subtype (get_type e1) rnd.fp_type && is_subtype (get_type e2) rnd.fp_type then
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
		  is_power_of_2_or_0 e2 && is_subtype (get_type e1) rnd.fp_type ->
		(* Eps = 0 *)
		Rounding ({rnd with eps_exp = 0}, arg)
	      | _ -> Rounding (rnd, arg)
	end 
  in
  simplify
  
		
