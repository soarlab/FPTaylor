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
           fprintf fmt "(%s)" (Num.string_of_num (Const.to_num c))
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
        | _ -> failwith "Info: unknown general operation"
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
        | _ -> failwith "OCamlInterval: unknown general operation"
      end
end

module Info = Make(InfoPrinter)
