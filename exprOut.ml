(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Symbolic expression printing                                               *)
(* -------------------------------------------------------------------------- *)

open Interval

module type PrinterType =
  sig
    val print : Format.formatter -> Expr.expr -> unit
  end

module type P =
  sig
    val print_fmt : Format.formatter -> Expr.expr -> unit
    val print_std : Expr.expr -> unit
    val print_str : Expr.expr -> string
  end

module Make(Printer : PrinterType) = struct
  let print_fmt = Printer.print
  let print_std = print_fmt Format.std_formatter
  let print_str = Lib.write_to_string print_fmt
end

                                       
module InfoPrinter : PrinterType = struct
  open Expr
  open Format
  
  let rec print fmt expr =
    match expr with
    | Const c -> begin
        match c with
        | Const.Rat n ->
           fprintf fmt "(%s)" (Num.string_of_num n)
        | Const.Interval v ->
           fprintf fmt "interval(%.20e, %.20e)" v.low v.high
      end
    | Var v -> fprintf fmt "%s" v
    | Rounding (rnd, arg) ->
       fprintf fmt "%s(%a)" (Rounding.rounding_to_string rnd) print arg
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(-(%a))" print arg
        | Op_abs -> fprintf fmt "abs(%a)" print arg
        | Op_inv -> fprintf fmt "inv(%a)" print arg
        | Op_sqrt -> fprintf fmt "sqrt(%a)" print arg
        | Op_exp -> fprintf fmt "exp(%a)" print arg
        | Op_log -> fprintf fmt "log(%a)" print arg
        | Op_sin -> fprintf fmt "sin(%a)" print arg
        | Op_cos -> fprintf fmt "cos(%a)" print arg
        | Op_tan -> fprintf fmt "tan(%a)" print arg
        | Op_asin -> fprintf fmt "asin(%a)" print arg
        | Op_acos -> fprintf fmt "acos(%a)" print arg
        | Op_atan -> fprintf fmt "atan(%a)" print arg
        | Op_sinh -> fprintf fmt "sinh(%a)" print arg
        | Op_cosh -> fprintf fmt "cosh(%a)" print arg
        | Op_tanh -> fprintf fmt "tanh(%a)" print arg                       
        | Op_asinh -> fprintf fmt "asinh(%a)" print arg
        | Op_acosh -> fprintf fmt "acosh(%a)" print arg
        | Op_atanh -> fprintf fmt "atanh(%a)" print arg
        | Op_floor_power2 -> fprintf fmt "floor_power2(%a)" print arg
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "min(%a, %a)" print arg1 print arg2
        | Op_max -> fprintf fmt "max(%a, %a)" print arg1 print arg2
        | Op_add -> fprintf fmt "(%a + %a)" print arg1 print arg2
        | Op_sub -> fprintf fmt "(%a - %a)" print arg1 print arg2
        | Op_mul -> fprintf fmt "(%a * %a)" print arg1 print arg2
        | Op_div -> fprintf fmt "(%a / %a)" print arg1 print arg2
        | Op_nat_pow -> fprintf fmt "(%a ^ %a)" print arg1 print arg2
        | Op_abs_err -> fprintf fmt "abs_err(%a, %a)" print arg1 print arg2
        | Op_sub2 -> fprintf fmt "sub2(%a, %a)" print arg1 print arg2
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | Op_fma, [a1; a2; a3] ->
           fprintf fmt "fma(%a, %a, %a)" print a1 print a2 print a3
        | _ -> failwith ("Info: unknown general operation: " ^ gen_op_name op)
      end
end


module OCamlIntervalPrinter : PrinterType = struct
  open Expr
  open Format
  
  let rec print fmt expr =
    match expr with
    | Const c ->
        let v = Const.to_interval c in
        fprintf fmt "{low = %.20e; high = %.20e}" v.low v.high
    | Var v -> fprintf fmt "var_%s" v
    | Rounding (rnd, arg) ->
       let rnd_str = Rounding.rounding_to_string rnd in
       failwith ("OCamlInterval: rounding is not allowed: " ^ rnd_str)
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(~-$(%a))" print arg
        | Op_abs -> fprintf fmt "abs_I(%a)" print arg
        | Op_inv -> fprintf fmt "inv_I(%a)" print arg
        | Op_sqrt -> fprintf fmt "sqrt_I(%a)" print arg
        | Op_exp -> fprintf fmt "exp_I(%a)" print arg
        | Op_log -> fprintf fmt "log_I(%a)" print arg
        | Op_sin -> fprintf fmt "sin_I(%a)" print arg
        | Op_cos -> fprintf fmt "cos_I(%a)" print arg
        | Op_tan -> fprintf fmt "tan_I(%a)" print arg
        | Op_asin -> fprintf fmt "asin_I(%a)" print arg
        | Op_acos -> fprintf fmt "acos_I(%a)" print arg
        | Op_atan -> fprintf fmt "atan_I(%a)" print arg
        | Op_sinh -> fprintf fmt "sinh_I(%a)" print arg
        | Op_cosh -> fprintf fmt "cosh_I(%a)" print arg
        | Op_tanh -> fprintf fmt "tanh_I(%a)" print arg                       
        | Op_asinh -> fprintf fmt "asinh_I(%a)" print arg
        | Op_acosh -> fprintf fmt "acosh_I(%a)" print arg
        | Op_atanh -> fprintf fmt "atanh_I(%a)" print arg
        | Op_floor_power2 -> fprintf fmt "floor_power2_I(%a)" print arg
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "min_I_I (%a) (%a)" print arg1 print arg2
        | Op_max -> fprintf fmt "max_I_I (%a) (%a)" print arg1 print arg2
        | Op_add -> fprintf fmt "(%a +$ %a)" print arg1 print arg2
        | Op_sub -> fprintf fmt "(%a -$ %a)" print arg1 print arg2
        | Op_mul -> fprintf fmt "(%a *$ %a)" print arg1 print arg2
        | Op_div -> fprintf fmt "(%a /$ %a)" print arg1 print arg2
        | Op_abs_err -> fprintf fmt "abs_err_I(%a, %a)" print arg1 print arg2
        | Op_sub2 -> fprintf fmt "sub2_I (%a, %a)" print arg1 print arg2
        | Op_nat_pow -> begin
            match arg2 with
            | Const (Const.Rat n) when Num.is_integer_num n ->
               fprintf fmt "pow_I_i (%a) (%s)" print arg1 (Num.string_of_num n)
            | _ ->
               failwith "OCamlInterval: Op_nat_pow: non-integer exponent"
          end
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | _ ->
           failwith ("OCamlInterval: unknown general operation: " ^ gen_op_name op)
      end
end


module RacketPrinter : PrinterType = struct
  open Expr
  open Format
  
  let rec print fmt expr =
    match expr with
    | Const c -> begin
        match c with
        | Const.Rat n ->
           fprintf fmt "(make-interval %s)" (Num.string_of_num n)
        | Const.Interval v ->
           let a = Num.string_of_num (More_num.num_of_float v.low) and
               b = Num.string_of_num (More_num.num_of_float v.high) in
           fprintf fmt "(make-interval %s %s)" a b
      end
    | Var v -> fprintf fmt "%s-var" v
    | Rounding (rnd, arg) ->
       let rnd_str = Rounding.rounding_to_string rnd in
       failwith ("Racket: rounding is not allowed: " ^ rnd_str)
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(i- %a)" print arg
        | Op_abs -> fprintf fmt "(iabs %a)" print arg
        | Op_inv -> fprintf fmt "(i/ %a)" print arg
        | Op_sqrt -> fprintf fmt "(isqrt %a)" print arg
        | Op_exp -> fprintf fmt "(iexp %a)" print arg
        | Op_log -> fprintf fmt "(ilog %a)" print arg
        | Op_sin -> fprintf fmt "(isin %a)" print arg
        | Op_cos -> fprintf fmt "(icos %a)" print arg
        | Op_tan -> fprintf fmt "(itan %a)" print arg
        | Op_asin -> fprintf fmt "(iasin %a)" print arg
        | Op_acos -> fprintf fmt "(iacos %a)" print arg
        | Op_atan -> fprintf fmt "(iatan %a)" print arg
        | Op_sinh -> fprintf fmt "(isinh %a)" print arg
        | Op_cosh -> fprintf fmt "(icosh %a)" print arg
        | Op_tanh -> fprintf fmt "(itanh %a)" print arg                       
        | Op_asinh -> fprintf fmt "(iasinh %a)" print arg
        | Op_acosh -> fprintf fmt "(iacosh %a)" print arg
        | Op_atanh -> fprintf fmt "(iatanh %a)" print arg
        | Op_floor_power2 -> fprintf fmt "(ifloor-pow2 %a)" print arg
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "(imin %a %a)" print arg1 print arg2
        | Op_max -> fprintf fmt "(imax %a %a)" print arg1 print arg2
        | Op_add -> fprintf fmt "(i+ %a %a)" print arg1 print arg2
        | Op_sub -> fprintf fmt "(i- %a %a)" print arg1 print arg2
        | Op_mul -> fprintf fmt "(i* %a %a)" print arg1 print arg2
        | Op_div -> fprintf fmt "(i/ %a %a)" print arg1 print arg2
        | Op_nat_pow -> fprintf fmt "(iexpt %a %a)" print arg1 print arg2
        | Op_abs_err -> fprintf fmt "(iabs-err %a %a)" print arg1 print arg2
        | Op_sub2 -> fprintf fmt "(isub2 %a %a)" print arg1 print arg2
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | Op_fma, [a1; a2; a3] ->
           fprintf fmt "(ifma %a %a %a)" print a1 print a2 print a3
        | _ -> failwith ("Racket: unknown general operation: " ^ gen_op_name op)
      end
end


module CPrinter : PrinterType = struct
  open Expr
  open Format
  
  let rec print fmt expr =
    match expr with
    | Const c -> fprintf fmt "(%.20e)" (Const.to_float c)
    | Var v -> fprintf fmt "var_%s" v
    | Rounding (rnd, arg) ->
       fprintf fmt "%s(%a)" (Rounding.rounding_to_string rnd) print arg
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(-(%a))" print arg
        | Op_abs -> fprintf fmt "fabs(%a)" print arg
        | Op_inv -> fprintf fmt "inv(%a)" print arg
        | Op_sqrt -> fprintf fmt "sqrt(%a)" print arg
        | Op_exp -> fprintf fmt "exp(%a)" print arg
        | Op_log -> fprintf fmt "log(%a)" print arg
        | Op_sin -> fprintf fmt "sin(%a)" print arg
        | Op_cos -> fprintf fmt "cos(%a)" print arg
        | Op_tan -> fprintf fmt "tan(%a)" print arg
        | Op_asin -> fprintf fmt "asin(%a)" print arg
        | Op_acos -> fprintf fmt "acos(%a)" print arg
        | Op_atan -> fprintf fmt "atan(%a)" print arg
        | Op_sinh -> fprintf fmt "sinh(%a)" print arg
        | Op_cosh -> fprintf fmt "cosh(%a)" print arg
        | Op_tanh -> fprintf fmt "tanh(%a)" print arg                       
        | Op_asinh -> fprintf fmt "asinh(%a)" print arg
        | Op_acosh -> fprintf fmt "acosh(%a)" print arg
        | Op_atanh -> fprintf fmt "atanh(%a)" print arg
        | Op_floor_power2 -> fprintf fmt "floor_power2(%a)" print arg
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "fmin(%a, %a)" print arg1 print arg2
        | Op_max -> fprintf fmt "fmax(%a, %a)" print arg1 print arg2
        | Op_add -> fprintf fmt "(%a + %a)" print arg1 print arg2
        | Op_sub -> fprintf fmt "(%a - %a)" print arg1 print arg2
        | Op_mul -> fprintf fmt "(%a * %a)" print arg1 print arg2
        | Op_div -> fprintf fmt "(%a / %a)" print arg1 print arg2
        | Op_nat_pow -> fprintf fmt "pow(%a, %a)" print arg1 print arg2
        | Op_abs_err -> fprintf fmt "abs_err(%a, %a)" print arg1 print arg2
        | Op_sub2 -> fprintf fmt "sub2(%a, %a)" print arg1 print arg2
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | Op_fma, [a1; a2; a3] ->
           fprintf fmt "fma(%a, %a, %a)" print a1 print a2 print a3
        | _ -> failwith ("C: unknown general operation: " ^ gen_op_name op)
      end
end


module OCamlFloatPrinter : PrinterType = struct
  open Expr
  open Format
  
  let rec print fmt expr =
    match expr with
    | Const c -> fprintf fmt "(%.20e)" (Const.to_float c)
    | Var v -> fprintf fmt "var_%s" v
    | Rounding (rnd, arg) ->
       let rnd_str = Rounding.rounding_to_string rnd in
       failwith ("OCamlFloat: rounding is not allowed: " ^ rnd_str)
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(-.(%a))" print arg
        | Op_abs -> fprintf fmt "abs_float(%a)" print arg
        | Op_inv -> fprintf fmt "(1. /. %a)" print arg
        | Op_sqrt -> fprintf fmt "sqrt(%a)" print arg
        | Op_exp -> fprintf fmt "exp(%a)" print arg
        | Op_log -> fprintf fmt "log(%a)" print arg
        | Op_sin -> fprintf fmt "sin(%a)" print arg
        | Op_cos -> fprintf fmt "cos(%a)" print arg
        | Op_tan -> fprintf fmt "tan(%a)" print arg
        | Op_asin -> fprintf fmt "asin(%a)" print arg
        | Op_acos -> fprintf fmt "acos(%a)" print arg
        | Op_atan -> fprintf fmt "atan(%a)" print arg
        | Op_sinh -> fprintf fmt "sinh(%a)" print arg
        | Op_cosh -> fprintf fmt "cosh(%a)" print arg
        | Op_tanh -> fprintf fmt "tanh(%a)" print arg                       
        | Op_asinh -> fprintf fmt "asinh(%a)" print arg
        | Op_acosh -> fprintf fmt "acosh(%a)" print arg
        | Op_atanh -> fprintf fmt "atanh(%a)" print arg
        | Op_floor_power2 -> fprintf fmt "floor_power2(%a)" print arg
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "min (%a) (%a)" print arg1 print arg2
        | Op_max -> fprintf fmt "max (%a) (%a)" print arg1 print arg2
        | Op_add -> fprintf fmt "(%a +. %a)" print arg1 print arg2
        | Op_sub -> fprintf fmt "(%a -. %a)" print arg1 print arg2
        | Op_mul -> fprintf fmt "(%a *. %a)" print arg1 print arg2
        | Op_div -> fprintf fmt "(%a /. %a)" print arg1 print arg2
        | Op_nat_pow -> fprintf fmt "(%a ** %a)" print arg1 print arg2
        | Op_abs_err -> fprintf fmt "abs_err(%a, %a)" print arg1 print arg2
        | Op_sub2 -> fprintf fmt "sub2(%a, %a)" print arg1 print arg2
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | _ ->
           failwith ("OCamlFloat: unknown general operation: " ^ gen_op_name op)
      end
end


module Z3PythonPrinter : PrinterType = struct
  open Expr
  open Format
  
  let rec print fmt expr =
    match expr with
    | Const c -> begin
        match c with
        | Const.Rat n ->
           let ns = Big_int.string_of_big_int (More_num.numerator n) and
               ds = Big_int.string_of_big_int (More_num.denominator n) in
           fprintf fmt "Q(%s, %s)" ns ds
        | Const.Interval v ->
           failwith "Z3Python: interval constants are not supported"
      end
    | Var v -> fprintf fmt "var_%s" v
    | Rounding (rnd, arg) ->
       let rnd_str = Rounding.rounding_to_string rnd in
       failwith ("Z3Python: rounding is not allowed: " ^ rnd_str)
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(-(%a))" print arg
        | Op_abs -> fprintf fmt "z3_abs(%a)" print arg
        | Op_inv -> fprintf fmt "inv(%a)" print arg
        | Op_sqrt -> fprintf fmt "sqrt(%a)" print arg
        | _ -> failwith ("Z3Python: unknown unary operation: " ^ u_op_name op)
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "z3_min(%a, %a)" print arg1 print arg2
        | Op_max -> fprintf fmt "z3_max(%a, %a)" print arg1 print arg2
        | Op_add -> fprintf fmt "(%a + %a)" print arg1 print arg2
        | Op_sub -> fprintf fmt "(%a - %a)" print arg1 print arg2
        | Op_mul -> fprintf fmt "(%a * %a)" print arg1 print arg2
        | Op_div -> fprintf fmt "(%a / %a)" print arg1 print arg2
        | Op_nat_pow -> fprintf fmt "(%a ** %a)" print arg1 print arg2
        | _ -> failwith ("Z3Python: unknown binary operation: " ^ bin_op_name op)
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | Op_fma, [a1; a2; a3] ->
           fprintf fmt "(%a * %a + %a)" print a1 print a2 print a3
        | _ -> failwith ("Z3Python: unknown general operation: " ^ gen_op_name op)
      end
end


module GelpiaPrinter : PrinterType = struct
  open Expr
  open Format
  
  let rec print fmt expr =
    match expr with
    | Const c ->
        let v = Const.to_interval c in
        fprintf fmt "interval(%.20e, %.20e)" v.low v.high
    | Var v -> fprintf fmt "var_%s" v
    | Rounding (rnd, arg) ->
       let rnd_str = Rounding.rounding_to_string rnd in
       failwith ("Gelpia: rounding is not allowed: " ^ rnd_str)
    | U_op (op, arg) -> begin
        match op with
        | Op_neg -> fprintf fmt "(-(%a))" print arg
        | Op_abs -> fprintf fmt "abs(%a)" print arg
        | Op_inv -> fprintf fmt "inv(%a)" print arg
        | Op_sqrt -> fprintf fmt "sqrt(%a)" print arg
        | Op_exp -> fprintf fmt "exp(%a)" print arg
        | Op_log -> fprintf fmt "log(%a)" print arg
        | Op_sin -> fprintf fmt "sin(%a)" print arg
        | Op_cos -> fprintf fmt "cos(%a)" print arg
        | Op_tan -> fprintf fmt "tan(%a)" print arg
        | Op_asin -> fprintf fmt "asin(%a)" print arg
        | Op_acos -> fprintf fmt "acos(%a)" print arg
        | Op_atan -> fprintf fmt "atan(%a)" print arg
        | Op_sinh -> fprintf fmt "sinh(%a)" print arg
        | Op_cosh -> fprintf fmt "cosh(%a)" print arg
        | Op_tanh -> fprintf fmt "tanh(%a)" print arg                       
        | Op_asinh -> fprintf fmt "asinh(%a)" print arg
        | Op_acosh -> fprintf fmt "acosh(%a)" print arg
        | Op_atanh -> fprintf fmt "atanh(%a)" print arg
        | Op_floor_power2 -> fprintf fmt "floor_power2(%a)" print arg
      end
    | Bin_op (op, arg1, arg2) -> begin
        match op with
        | Op_min -> fprintf fmt "min(%a, %a)" print arg1 print arg2
        | Op_max -> fprintf fmt "max(%a, %a)" print arg1 print arg2
        | Op_add -> fprintf fmt "(%a + %a)" print arg1 print arg2
        | Op_sub -> fprintf fmt "(%a - %a)" print arg1 print arg2
        | Op_mul -> fprintf fmt "(%a * %a)" print arg1 print arg2
        | Op_div -> fprintf fmt "(%a / %a)" print arg1 print arg2
        | Op_abs_err -> fprintf fmt "abs_err(%a, %a)" print arg1 print arg2
        | Op_sub2 -> fprintf fmt "sub2(%a, %a)" print arg1 print arg2
        | Op_nat_pow -> begin
            match arg2 with
            | Const (Const.Rat n) when Num.is_integer_num n ->
               fprintf fmt "pow(%a, %s)" print arg1 (Num.string_of_num n)
            | _ ->
               failwith "Gelpia: Op_nat_pow: non-integer exponent"
          end
      end
    | Gen_op (op, args) -> begin
        match (op, args) with
        | _ ->
           failwith ("Gelpia: unknown general operation: " ^ gen_op_name op)
      end
end


module Info = Make(InfoPrinter)
