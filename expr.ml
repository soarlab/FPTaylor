(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Symbolic expressions                                                       *)
(* -------------------------------------------------------------------------- *)

open Num
open List
open Lib
open Interval
open Rounding

(* Operations *)
type op_type = 
  | Op_neg
  | Op_abs
  | Op_max
  | Op_min
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_inv
  | Op_sqrt
  | Op_sin
  | Op_cos
  | Op_tan
  | Op_asin
  | Op_acos
  | Op_atan
  | Op_exp
  | Op_log
  | Op_sinh
  | Op_cosh
  | Op_tanh
  | Op_asinh
  | Op_acosh
  | Op_atanh
  | Op_fma
  | Op_nat_pow
  | Op_sub2
  | Op_abs_err
  | Op_floor_power2

(* Expression *)
type expr =
  | Const of Const.t
  | Var of string
  | Rounding of rnd_info * expr
  | U_op of op_type * expr
  | Bin_op of op_type * expr * expr
  | Gen_op of op_type * expr list

type formula =
  | Le of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr

let mk_const c = Const c and
    mk_var v = Var v and
    mk_rounding rnd a = Rounding (rnd, a) and
    mk_neg a = U_op (Op_neg, a) and
    mk_abs a = U_op (Op_abs, a) and
    mk_sqrt a = U_op (Op_sqrt, a) and
    mk_inv a = U_op (Op_inv, a) and
    mk_sin a = U_op (Op_sin, a) and
    mk_cos a = U_op (Op_cos, a) and
    mk_tan a = U_op (Op_tan, a) and
    mk_asin a = U_op (Op_asin, a) and
    mk_acos a = U_op (Op_acos, a) and
    mk_atan a = U_op (Op_atan, a) and
    mk_exp a = U_op (Op_exp, a) and
    mk_log a = U_op (Op_log, a) and
    mk_sinh a = U_op (Op_sinh, a) and
    mk_cosh a = U_op (Op_cosh, a) and
    mk_tanh a = U_op (Op_tanh, a) and
    mk_asinh a = U_op (Op_asinh, a) and
    mk_acosh a = U_op (Op_acosh, a) and
    mk_atanh a = U_op (Op_atanh, a) and
    mk_max a b = Bin_op (Op_max, a, b) and
    mk_min a b = Bin_op (Op_min, a, b) and
    mk_add a b = Bin_op (Op_add, a, b) and
    mk_sub a b = Bin_op (Op_sub, a, b) and
    mk_mul a b = Bin_op (Op_mul, a, b) and
    mk_div a b = Bin_op (Op_div, a, b) and
    mk_nat_pow a b = Bin_op (Op_nat_pow, a, b) and
    mk_fma a b c = Gen_op (Op_fma, [a; b; c]) and
    mk_sub2 a b = Bin_op (Op_sub2, a, b) and
    mk_abs_err t x = Bin_op (Op_abs_err, t, x) and
    mk_floor_power2 a = U_op (Op_floor_power2, a)

let mk_int_const i = mk_const (Const.of_int i) and
    mk_num_const n = mk_const (Const.of_num n) and
    mk_float_const f = mk_const (Const.of_float f) and
    mk_interval_const v = mk_const (Const.of_interval v)
                                   
let mk_floor_sub2 a b = mk_floor_power2 (mk_sub2 a b)

let const_0 = mk_int_const 0 and
    const_1 = mk_int_const 1 and
    const_2 = mk_int_const 2 and
    const_3 = mk_int_const 3 and
    const_4 = mk_int_const 4 and
    const_5 = mk_int_const 5

let rec eq_expr e1 e2 =
  match (e1, e2) with
    | (Const c1, Const c2) -> Const.eq_c c1 c2
    | (Var v1, Var v2) -> v1 = v2
    | (Rounding (r1, a1), Rounding (r2, a2)) when r1 = r2 -> 
      eq_expr a1 a2
    | (U_op (t1, a1), U_op (t2, a2)) when t1 = t2 -> 
      eq_expr a1 a2
    | (Bin_op (t1, a1, b1), Bin_op (t2, a2, b2)) when t1 = t2 ->
      eq_expr a1 a2 && eq_expr b1 b2
    | (Gen_op (t1, as1), Gen_op (t2, as2)) when t1 = t2 ->
      itlist (fun (a1, a2) x -> eq_expr a1 a2 && x) (zip as1 as2) true
    | _ -> false

let rec vars_in_expr e =
  match e with
    | Var v -> [v]
    | Rounding (_, a1) ->
      vars_in_expr a1
    | U_op (_, a1) -> 
      vars_in_expr a1
    | Bin_op (_, a1, a2) ->
      union (vars_in_expr a1) (vars_in_expr a2)
    | Gen_op (_, args) ->
      let vs = map vars_in_expr args in
      itlist union vs []
    | _ -> []

type print_env = {
  env_op_name : op_type -> bool * string;
  env_op_infix : op_type -> bool * bool;
  env_print : (string -> unit) -> (expr -> unit) -> expr -> bool
}

let def_print_env = {
  env_op_name = (fun _ -> false, "");
  env_op_infix = (fun _ -> false, false);
  env_print = (fun _ _ _ -> false);
}

let op_name_in_env env op =
  let b, str = env.env_op_name op in
  if b then str else
    match op with
      | Op_neg -> "-"
      | Op_abs -> "abs"
      | Op_max -> "max"
      | Op_min -> "min"
      | Op_add -> "+"
      | Op_sub -> "-"
      | Op_mul -> "*"
      | Op_div -> "/"
      | Op_inv -> "inv"
      | Op_sqrt -> "sqrt"
      | Op_sin -> "sin"
      | Op_cos -> "cos"
      | Op_tan -> "tan"
      | Op_asin -> "asin"
      | Op_acos -> "acos"
      | Op_atan -> "atan"
      | Op_exp -> "exp"
      | Op_log -> "log"
      | Op_sinh -> "sinh"
      | Op_cosh -> "cosh"
      | Op_tanh -> "tanh"
      | Op_asinh -> "asinh"
      | Op_acosh -> "acosh"
      | Op_atanh -> "atanh"
      | Op_fma -> "fma"
      | Op_nat_pow -> "^" 
      | Op_sub2 -> "sub2"
      | Op_abs_err -> "abs_err"
      | Op_floor_power2 -> "floor_power2"

let is_infix_in_env env op =
  let b, r = env.env_op_infix op in
  if b then r else
    match op with
      | Op_add | Op_sub | Op_mul | Op_div | Op_nat_pow -> true
      | _ -> false

let op_name = op_name_in_env def_print_env

let is_infix = is_infix_in_env def_print_env

let c_print_env = {
  env_op_name = (function
    | Op_abs -> true, "fabs"
    | Op_nat_pow -> true, "pow"
    | _ -> false, "");

  env_op_infix = (function
    | Op_nat_pow -> true, false
    | _ -> false, false);

  env_print = (fun p _ e ->
    match e with
      | Const c -> 
	let _ = p ("(" ^ string_of_float (Const.to_float c) ^ ")") in
	true
      | _ -> false);
}

let gelpia_print_env = {
  env_op_name = (function
    | Op_nat_pow -> true, "pow"
    | _ -> false, "");

  env_op_infix = (function
    | Op_nat_pow -> true, false
    | _ -> false, false);

  env_print = (fun p print e ->
    match e with
      | Const c ->
         let v = Const.to_interval c in
	 let _ = p (Format.sprintf "interval(%.20e, %.20e)" v.low v.high) in
	 true
      | Bin_op (Op_nat_pow, arg1, arg2) ->
	begin
	  match arg2 with
	  | Const c -> begin
	       let n = try Const.to_num c
                       with Failure _ -> failwith "Op_nat_pow: interval exponent" in
	       if is_integer_num n && n >/ Int 0 then
		 let _ =
		   p "pow("; print arg1; p ", ";
		   p (string_of_num n); p ")" in
		 true
	       else
		 failwith "Op_nat_pow: non-integer exponent"
            end
	  | _ -> failwith "Op_nat_pow: non-constant exponent"
	end
      | _ -> false);
}

let z3py_print_env = {
  env_op_name = (fun op ->
    match op with
      | Op_nat_pow -> true, "**"
      | Op_abs -> true, "z3_abs"
      | Op_min -> true, "z3_min"
      | Op_max -> true, "z3_max"
      | Op_sin | Op_cos | Op_tan | Op_asin | Op_acos | Op_atan 
      | Op_exp | Op_log 
      | Op_sinh | Op_cosh | Op_tanh | Op_asinh | Op_acosh | Op_atanh
      | Op_sub2 | Op_floor_power2 | Op_abs_err
	-> failwith ("z3py: " ^ op_name op ^ " is not supported")
      | _ -> false, "");

  env_op_infix = (function
    | _ -> false, false);

  env_print = (fun p _ e ->
    match e with
    | Const c -> begin
        let n = try Const.to_num c
                with Failure _ -> failwith "z3py: interval constants are not supported" in
	let s = Big_int.string_of_big_int in
	let ns = s (More_num.numerator n) and
	    ds = s (More_num.denominator n) in
(*	let _ = p ("(" ^ string_of_num f.rational_v ^ ")") in *)
	let _ = p ("(Q(" ^ ns ^ "," ^ ds ^ "))") in
	true
      end
    | _ -> false);
}

let ocaml_float_print_env = {
  env_op_name = (function
    | Op_neg -> true, "-."
    | Op_add -> true, "+."
    | Op_sub -> true, "-."
    | Op_mul -> true, "*."
    | Op_div -> true, "/."
    | Op_abs -> true, "abs_float"
    | Op_max -> true, "(fun (x, y) -> max x y)"
    | Op_min -> true, "(fun (x, y) -> min x y)"
    | Op_nat_pow -> true, "**"
    | Op_sub2 -> true, "sub2"
    | Op_abs_err -> true, "abs_err"
    | Op_floor_power2 -> true, "floor_power2"
    | _ -> false, "");

  env_op_infix = (function
    | _ -> false, false);

  env_print = (fun p _ e ->
    match e with
      | Var v -> let _ = p ("var_" ^ v) in true
      | Const c -> 
	let _ = p ("(" ^ string_of_float (Const.to_float c) ^ ")") in
	true
      | _ -> false);
}

let racket_interval_env_op_name = function
  | Op_neg -> true, "i-"
  | Op_add -> true, "i+"
  | Op_sub -> true, "i-"
  | Op_mul -> true, "i*"
  | Op_div -> true, "i/"
  | Op_abs -> true, "iabs"
  | Op_inv -> true, "i/"
  | Op_sqrt -> true, "isqrt"
  | Op_sin -> true, "isin"
  | Op_cos -> true, "icos"
  | Op_tan -> true, "itan"
  | Op_asin -> true, "iasin"
  | Op_acos -> true, "iacos"
  | Op_atan -> true, "iatan"
  | Op_exp -> true, "iexp"
  | Op_log -> true, "ilog"
  | Op_sinh -> true, "isinh"
  | Op_cosh -> true, "icosh"
  | Op_tanh -> true, "itanh"
  | Op_asinh -> true, "iasinh"
  | Op_acosh -> true, "iacosh"
  | Op_atanh -> true, "iatanh"
  | Op_nat_pow -> true, "iexpt"
  | Op_sub2 -> true, "isub2"
  | Op_floor_power2 -> true, "ifloor-pow2"
  | _ -> false, ""

let racket_interval_print_env = {
  env_op_name = racket_interval_env_op_name;

  env_op_infix = (function
    | _ -> false, false);

  env_print = (fun p print e ->
    let op_name op =
      let flag, name = racket_interval_env_op_name op in
      if (not flag) then
	"unknown"
      else
	name in
    let _ =
      match e with
	| Var name -> p (name ^ "-var")
	| Const c -> 
	   let s = Big_int.string_of_big_int in
           let a, b =
             if Const.is_rat_const c then
	       let n = Const.to_num c in
               n, n
             else
               let v = Const.to_interval c in
               More_num.num_of_float v.low, More_num.num_of_float v.high in
	   let na = s (More_num.numerator a) and
	       da = s (More_num.denominator a) and
               nb = s (More_num.numerator b) and
               db = s (More_num.denominator b) in
	   p (Format.sprintf "(make-interval %s/%s %s/%s)" na da nb db)
	| U_op (op, arg) ->
	  begin
	    p "("; 
	    p (op_name op); 
	    p " ";
	    print arg;
	    p ")"
	  end
	| Bin_op (op, arg1, arg2) ->
	  begin
	    p "(";
	    p (op_name op);
	    p " ";
	    print arg1;
	    p " ";
	    print arg2;
	    p ")"
	  end
	| Gen_op (op, args) ->
	  begin
	    p "(";
	    p (op_name op);
	    p " ";
	    print_list print (fun () -> p " ") args;
	    p ")"
	  end
	| Rounding _ ->
	  failwith "Racket environment: rounding is not supported"
    in true);
}


let print_expr_in_env env fmt =
  let p = Format.pp_print_string fmt in
  let rec print e =
    let b = env.env_print p print e in
    if b then () else
      match e with
        | Const c ->
           if Const.is_rat_const c then
             p ("(" ^ string_of_num (Const.to_num c) ^ ")")
           else
             let v = Const.to_interval c in
             p (Format.sprintf "interval(%.20e, %.20e)" v.low v.high)
	| Var v -> p v
	| Rounding (rnd, arg) ->
	  begin
	    p (rounding_to_string rnd);
	    p "("; print arg; p ")";
	  end
	| U_op (op, arg) ->
	  begin
	    p "(";
	    p (op_name_in_env env op);
	    p "("; print arg; p ")";
	    p ")";
	  end
	| Bin_op (op, arg1, arg2) ->
	  let name = op_name_in_env env op in
	  if is_infix_in_env env op then
	    begin
	      p "(";
	      print arg1;
	      p " "; p name; p " ";
	      print arg2;
	      p ")";
	    end
	  else
	    begin
	      p name;
	      p "(";
	      print arg1;
	      p ", ";
	      print arg2;
	      p ")";
	    end
	| Gen_op (op, args) -> 
	  let name = op_name_in_env env op in
	  begin
	    p name;
	    p "(";
	    print_list print (fun () -> p ", ") args;
	    p ")";
	  end
  in
  print

let print_expr_str_in_env env = write_to_string (print_expr_in_env env)


(* 
* Returns true if the given constant value can be reprsented exactly
* with a floating point number 
*)
let is_fp_exact eps c =
  let v = Const.to_interval c in
  if v.low <> v.high then
    false
  else
    let () = if eps <> 2.0 ** (-53.0) then
               Log.warning 0 "is_fp_exact: possible inexact result for eps <> eps64" in
    true
	  
