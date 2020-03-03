(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Numerical (float, rational, interval) evaluation of symbolic expressions   *)
(* -------------------------------------------------------------------------- *)

open Interval
open Num
open Binary_float
open Expr

(* Computes a floating-point value of an expression *)
(* vars : string -> float is a function which associates 
   floating-point values with variable names *)
(*
input: string->float
output: float
description: evaluate an expression and return a float value, the first argument is a function which return the value (float) of a variable's name
*)
let eval_float_expr vars =
  let rec eval = function
    | Const c -> Const.to_float c
    | Var v -> vars v
    | Rounding _ as expr -> failwith ("eval_float_expr: Rounding is not supported: " ^
                                      ExprOut.Info.print_str expr)
    | U_op (op, arg) ->
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
        | Op_asin -> asin x
        | Op_acos -> acos x
        | Op_atan -> atan x
        | Op_exp -> exp x
        | Op_log -> log x
        | Op_sinh -> sinh x
        | Op_cosh -> cosh x
        | Op_tanh -> tanh x
        | Op_asinh -> Func.asinh x
        | Op_acosh -> Func.acosh x
        | Op_atanh -> Func.atanh x
        | Op_floor_power2 -> Func.floor_power2 x
      end
    | Bin_op (op, arg1, arg2) ->
      begin
        let x1 = eval arg1 and
        x2 = eval arg2 in
        match op with
        | Op_add -> x1 +. x2
        | Op_sub -> x1 -. x2
        | Op_mul -> x1 *. x2
        | Op_div -> x1 /. x2
        | Op_max -> max x1 x2
        | Op_min -> min x1 x2
        | Op_nat_pow -> x1 ** x2
        | Op_sub2 -> Func.sub2 (x1, x2)
        | Op_abs_err -> Func.abs_err (x1, x2)
      end
    | Gen_op (op, args) ->
      begin
        let xs = List.map eval args in
        match (op, xs) with
        | (Op_fma, [a;b;c]) -> a *. b +. c
        | _ -> failwith ("eval_float_expr: Unsupported general operation: " 
                         ^ gen_op_name op)
      end
  in
  eval

(*
input: expr
output: float
description: estimate a constant expression and return a float value
*)
let eval_float_const_expr =
  eval_float_expr (fun v -> failwith ("eval_float_const_expr: Var " ^ v))

(* Computes a rational value of an expression *)
(* vars : string -> num is a function which associates 
   rational values with variable names *)
(*
input: string->num
output: num
description: estimate an expression and return a float value, the first argument is a function which return the value (num) of a variable's name
*)
let eval_num_expr vars =
  let one = Int 1 in
  let rec eval = function
    | Const c -> begin
        try Const.to_num c
        with Failure _ -> failwith "eval_num_expr: interval constant"
      end
    | Var v -> vars v
    | Rounding (rnd, arg) -> round_num rnd (eval arg)
    | U_op (op, arg) ->
      begin
        let x = eval arg in
        match op with
        | Op_neg -> minus_num x
        | Op_abs -> abs_num x
        | Op_inv -> one // x
        | _ -> failwith ("eval_num_expr: Unsupported unary operation: " 
                         ^ u_op_name op)
      end
    | Bin_op (op, arg1, arg2) ->
      begin
        let x1 = eval arg1 and
        x2 = eval arg2 in
        match op with
        | Op_add -> x1 +/ x2
        | Op_sub -> x1 -/ x2
        | Op_mul -> x1 */ x2
        | Op_div -> x1 // x2
        | Op_max -> max_num x1 x2
        | Op_min -> min_num x1 x2
        | Op_nat_pow -> x1 **/ x2
        | _ -> failwith ("eval_num_expr: Unsupported binary operation: " 
                         ^ bin_op_name op)
      end
    | Gen_op (op, args) ->
      begin
        let xs = List.map eval args in
        match (op, xs) with
        | (Op_fma, [a;b;c]) -> (a */ b) +/ c
        | _ -> failwith ("eval_num_expr: Unsupported general operation: " 
                         ^ gen_op_name op)
      end
  in
  eval

(*
input: expr
output: num
description: estimate a constant expression and return a num value
*)
let eval_num_const_expr =
  eval_num_expr (fun v -> failwith ("eval_num_const_expr: Var " ^ v))

(* Computes an interval value of an expression *)
(* vars : string -> interval is a function which associates 
   inteval values with variable names *)
(*
input: string->interval
output: interval
description: estimate an expression and return a float value, the first argument is a function which return the value (interval) of a variable's name
*)
let eval_interval_expr vars =
  let rec eval = function
    | Const c -> Const.to_interval c
    | Var v -> vars v
    | Rounding _ as expr -> failwith ("eval_interval_expr: Rounding is not supported: " ^
                                      ExprOut.Info.print_str expr)
    | U_op (op, arg) ->
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
        | Op_asin -> asin_I x
        | Op_acos -> acos_I x
        | Op_atan -> atan_I x
        | Op_exp -> exp_I x
        | Op_log -> log_I x
        | Op_sinh -> sinh_I x
        | Op_cosh -> cosh_I x
        | Op_tanh -> tanh_I x
        | Op_asinh -> Func.asinh_I x
        | Op_acosh -> Func.acosh_I x
        | Op_atanh -> Func.atanh_I x
        | Op_floor_power2 -> Func.floor_power2_I x
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
        | Op_div -> x1 /$ eval arg2
        | Op_max -> max_I_I x1 (eval arg2)
        | Op_min -> min_I_I x1 (eval arg2)
        | Op_nat_pow -> x1 **$. (eval_float_const_expr arg2)
        | Op_sub2 -> Func.sub2_I (x1, eval arg2)
        | Op_abs_err -> Func.abs_err_I (x1, eval arg2)
      end
    | Gen_op (op, args) ->
      begin
        let xs = List.map eval args in
        match (op, xs) with
        | (Op_fma, [a;b;c]) -> (a *$ b) +$ c
        | _ -> failwith ("eval_interval_expr: Unsupported general operation: " 
                         ^ gen_op_name op)
      end
  in
  eval

(*
input: expr
output: interval
description: estimate a constant expression and return an interval value
*)
let eval_interval_const_expr =
  eval_interval_expr (fun v -> failwith ("eval_interval_const_expr: Var " ^ v))

(*
input: expr
output: Const.t
description: estimate a constant expression and return a constant
*)
let eval_const_expr e = 
  Log.report `Debug "eval_const_expr: %s" (ExprOut.Info.print_str e);
  let n = eval_num_const_expr e in
  Log.report `Debug "result: %s" (string_of_num n);
  Const.of_num n
