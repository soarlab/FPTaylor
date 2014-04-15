(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Num
open Lib
(* open Interval *)

type evaluated_const = {
  rational_v : num;
  float_v : float;
(*  interval_v : interval; *)
}

(* Operations *)
type op_type = 
  | Op_neg
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_inv
  | Op_nat_pow

type op_flags = {
  op_exact : bool;
}

let op_name op flags =
  let name =
    match op with
      | Op_neg -> "-"
      | Op_add -> "+"
      | Op_sub -> "-"
      | Op_mul -> "*"
      | Op_div -> "/"
      | Op_inv -> "/"
      | Op_nat_pow -> "^" 
  in
  if flags.op_exact then "$" ^ name else name

let is_infix op =
  match op with
    | Op_add | Op_mul | Op_div | Op_nat_pow -> true
    | _ -> false

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


let print_expr fmt =
  let p = Format.pp_print_string fmt in
  let rec print = function
    | Const f -> p (string_of_num f.rational_v)
    | Var v -> p v
    | U_op (op, flags, arg) ->
      begin
	p "(";
	p (op_name op flags);
	print arg;
	p ")"
      end
    | Bin_op (op, flags, arg1, arg2) ->
      let name = op_name op flags in
      if is_infix op then
	begin
	  p "(";
	  print arg1;
	  p name;
	  print arg2;
	  p ")"
	end
      else
	begin
	  p name;
	  p "(";
	  print arg1;
	  p ", ";
	  print arg2;
	  p ")"
	end
    | Gen_op (op, flags, args) -> failwith "Gen_op: not implemented"
  in
  print

let print_expr_std = print_expr Format.std_formatter
let print_expr_str = print_to_string print_expr

(*
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

*)
