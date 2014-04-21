(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Num
open List
open Lib
open Interval

type evaluated_const = {
  rational_v : num;
  float_v : float;
  interval_v : interval;
}

(* Operations *)
type op_type = 
  | Op_neg
  | Op_abs
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_inv
  | Op_sqrt
  | Op_sin
  | Op_cos
  | Op_tan
  | Op_exp
  | Op_log
  | Op_fma
  | Op_nat_pow

type op_flags = {
  op_exact : bool;
}

(* Expression *)
type expr =
  | Const of evaluated_const
  | Var of string
  | U_op of op_type * op_flags * expr
  | Bin_op of op_type * op_flags * expr * expr
  | Gen_op of op_type * op_flags * expr list

type formula =
  | Le of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr

let mk_neg f a = U_op (Op_neg, f, a) and
    mk_abs f a = U_op (Op_abs, f, a) and
    mk_sqrt f a = U_op (Op_sqrt, f, a) and
    mk_inv f a = U_op (Op_inv, f, a) and
    mk_sin f a = U_op (Op_sin, f, a) and
    mk_cos f a = U_op (Op_cos, f, a) and
    mk_tan f a = U_op (Op_tan, f, a) and
    mk_exp f a = U_op (Op_exp, f, a) and
    mk_log f a = U_op (Op_log, f, a) and
    mk_add f a b = Bin_op (Op_add, f, a, b) and
    mk_sub f a b = Bin_op (Op_sub, f, a, b) and
    mk_mul f a b = Bin_op (Op_mul, f, a, b) and
    mk_div f a b = Bin_op (Op_div, f, a, b) and
    mk_nat_pow f a b = Bin_op (Op_nat_pow, f, a, b) and
    mk_fma f a b c = Gen_op (Op_fma, f, [a; b; c])

let rec eq_expr e1 e2 =
  match (e1, e2) with
    | (Const c1, Const c2) ->
      c1.rational_v =/ c2.rational_v
    | (Var v1, Var v2) -> v1 = v2
    | (U_op (t1, f1, a1), U_op (t2, f2, a2)) 
	when t1 = t2 && f1 = f2 ->
      eq_expr a1 a2
    | (Bin_op (t1, f1, a1, b1), Bin_op (t2, f2, a2, b2)) 
	when t1 = t2 && f1 = f2 ->
      eq_expr a1 a2 && eq_expr b1 b2
    | (Gen_op (t1, f1, as1), Gen_op (t2, f2, as2))
	when t1 = t2 && f1 = f2 ->
      itlist (fun (a1, a2) x -> eq_expr a1 a2 && x) (zip as1 as2) true
    | _ -> false

let rec vars_in_expr e =
  match e with
    | Var v -> [v]
    | U_op (_, _, a1) -> 
      vars_in_expr a1
    | Bin_op (_, _, a1, a2) ->
      union (vars_in_expr a1) (vars_in_expr a2)
    | Gen_op (_, _, args) ->
      let vs = map vars_in_expr args in
      itlist union vs []
    | _ -> []

type print_env = {
  env_op_name : op_type -> bool * string;
  env_op_infix : op_type -> bool * bool;
  env_print : expr -> bool * string;
}

let def_print_env = {
  env_op_name = (fun _ -> false, "");
  env_op_infix = (fun _ -> false, false);
  env_print = (fun _ -> false, "");
}

let op_name_in_env env op flags =
  let b, str = env.env_op_name op in
  if b then str else
    let name = 
      match op with
	| Op_neg -> "-"
	| Op_abs -> "abs"
	| Op_add -> "+"
	| Op_sub -> "-"
	| Op_mul -> "*"
	| Op_div -> "/"
	| Op_inv -> "/"
	| Op_sqrt -> "sqrt"
	| Op_sin -> "sin"
	| Op_cos -> "cos"
	| Op_tan -> "tan"
	| Op_exp -> "exp"
	| Op_log -> "log"
	| Op_fma -> "fma"
	| Op_nat_pow -> "^" 
    in
    if flags.op_exact then "$" ^ name else name

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

  env_print = (function
    | Const f -> true, string_of_float f.float_v
    | _ -> false, "");
}

let z3py_print_env = {
  env_op_name = (fun op ->
    match op with
      | Op_nat_pow -> true, "**"
      | Op_abs | Op_sin | Op_cos | Op_tan | Op_exp | Op_log
	-> failwith ("z3py: " ^ op_name op {op_exact = false} ^ " is not supported")
      | _ -> false, "");

  env_op_infix = (function
    | _ -> false, false);

  env_print = (function
    | Const f -> true, string_of_num f.rational_v
    | _ -> false, "");
}


let print_expr_in_env env fmt =
  let p = Format.pp_print_string fmt in
  let rec print e =
    let b, str = env.env_print e in
    if b then p str else
      match e with
	| Const f -> p (string_of_num f.rational_v)
	| Var v -> p v
	| U_op (op, flags, arg) ->
	  begin
	    p "(";
	    p (op_name_in_env env op flags);
	    print arg;
	    p ")";
	  end
	| Bin_op (op, flags, arg1, arg2) ->
	  let name = op_name_in_env env op flags in
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
	| Gen_op (op, flags, args) -> 
	  let name = op_name_in_env env op flags in
	  begin
	    p name;
	    p "(";
	    print_list print (fun () -> p ", ") args;
	    p ")";
	  end
  in
  print

let print_expr = print_expr_in_env def_print_env

let print_expr_std = print_expr Format.std_formatter
let print_expr_str = print_to_string print_expr

let print_expr_str_in_env env = print_to_string (print_expr_in_env env)


(*
  Returns true if the given constant value can be reprsented exactly
  with a floating point number
*)
let is_fp_exact eps c =
  if c.interval_v.low <> c.float_v || c.interval_v.high <> c.float_v then
    false
  else
    let _ = issue_warning (eps <> 2.0 ** (-53.0))
      "is_fp_exact: possible inexact result for eps <> eps64" in
    true
	  
let const_of_num n = {
  rational_v = n;
  float_v = float_of_num n;
  interval_v = More_num.interval_of_num n;
}

let const_of_int n = const_of_num (num_of_int n)


let expr_0 = Const (const_of_int 0)
let expr_1 = Const (const_of_int 1)
let expr_2 = Const (const_of_int 2)
let expr_3 = Const (const_of_int 3)
let expr_4 = Const (const_of_int 4)
let expr_5 = Const (const_of_int 5)


