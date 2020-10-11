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

(* Operations *)
type u_op_type = 
  | Op_neg
  | Op_abs
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
  | Op_floor_power2

type bin_op_type =
  | Op_max
  | Op_min
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_nat_pow
  | Op_sub2
  | Op_abs_err

type gen_op_type =
  | Op_fma
  | Op_ulp

(* Expression *)
type expr =
  | Const of Const.t
  | Var of string
  | Rounding of Rounding.rnd_info * expr
  | U_op of u_op_type * expr
  | Bin_op of bin_op_type * expr * expr
  | Gen_op of gen_op_type * expr list

type formula =
  | Le of expr * expr
  | Lt of expr * expr
  | Eq of expr * expr

type constraints = {
  var_interval : string -> Interval.interval;
  var_rat_bounds : string -> Num.num * Num.num;
  var_uncertainty : string -> Const.t;
  constraints : formula list;
}

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

let mk_ulp (prec, e_min) x = 
  let p = mk_const (Const.of_int prec) in
  let e = mk_const (Const.of_int e_min) in
  Gen_op (Op_ulp, [p; e; x])

let mk_int_const i = mk_const (Const.of_int i)
let mk_num_const n = mk_const (Const.of_num n)
let mk_float_const f = mk_const (Const.of_float f)
let mk_interval_const v = mk_const (Const.of_interval v)

let mk_floor_sub2 a b = mk_floor_power2 (mk_sub2 a b)

let const_0 = mk_int_const 0 and
  const_1 = mk_int_const 1 and
  const_2 = mk_int_const 2 and
  const_3 = mk_int_const 3 and
  const_4 = mk_int_const 4 and
  const_5 = mk_int_const 5

let u_op_name = function
  | Op_neg -> "neg"
  | Op_abs -> "abs"
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
  | Op_floor_power2 -> "floor_power2"

let bin_op_name = function
  | Op_max -> "max"
  | Op_min -> "min"
  | Op_add -> "+"
  | Op_sub -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"
  | Op_nat_pow -> "^" 
  | Op_sub2 -> "sub2"
  | Op_abs_err -> "abs_err"

let gen_op_name = function
  | Op_fma -> "fma"
  | Op_ulp -> "ulp"

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
    Lib.itlist (fun (a1, a2) x -> eq_expr a1 a2 && x) (Lib.zip as1 as2) true
  | _ -> false

module ExprHashtbl = Hashtbl.Make (
  struct
    type t = expr
    let equal e1 e2 = eq_expr e1 e2
    (* TODO: is it possible that eq_expr e1 e2 = true but hashes are different? *)
    let hash e = Hashtbl.hash e
  end)

let rec vars_in_expr e =
  match e with
  | Var v -> [v]
  | Rounding (_, a1) ->
    vars_in_expr a1
  | U_op (_, a1) -> 
    vars_in_expr a1
  | Bin_op (_, a1, a2) ->
    Lib.union (vars_in_expr a1) (vars_in_expr a2)
  | Gen_op (_, args) ->
    let vs = List.map vars_in_expr args in
    Lib.itlist Lib.union vs []
  | _ -> []

let is_ref_var = function
| Var v when Lib.starts_with v "ref~" -> true
| _ -> false

let mk_ref_var i = Var ("ref~" ^ string_of_int i)

let index_of_ref_var = function
| Var v when Lib.starts_with v "ref~" -> int_of_string (Lib.slice v 4)
| _ -> failwith "ref_var_index: not a reference"

(* Finds common subexpressions and returns a list of expressions
   with references *)
let expr_ref_list_of_expr ex =
  let hc = ExprHashtbl.create 128 in
  let hi = ExprHashtbl.create 128 in
  let get_count ex = try ExprHashtbl.find hc ex with Not_found -> 0 in
  let incr_count ex = ExprHashtbl.replace hc ex (1 + get_count ex) in
  let get_index ex = try ExprHashtbl.find hi ex with Not_found -> -1 in
  let set_index ex i = ExprHashtbl.add hi ex i in
  let rec count ex =
    incr_count ex;
    match ex with
    | Rounding (_, arg) -> count arg
    | U_op (_, arg) -> count arg
    | Bin_op (_, arg1, arg2) -> count arg1; count arg2
    | Gen_op (_, args) -> List.iter count args
    | _ -> ()
  in
  let rec find_common acc ex =
    match ex with
    | Const _ | Var _ -> acc, ex
    | _ ->
      let acc, ex' =
        match ex with
        | Rounding (rnd, arg) ->
          let acc, arg = find_common acc arg in
          acc, Rounding (rnd, arg)
        | U_op (op, arg) ->
          let acc, arg = find_common acc arg in
          acc, U_op (op, arg)
        | Bin_op (op, arg1, arg2) ->
          let acc, arg1 = find_common acc arg1 in
          let acc, arg2 = find_common acc arg2 in
          acc, Bin_op (op, arg1, arg2)
        | Gen_op (op, args) ->
          let acc, args = List.fold_left (fun (acc, args) arg ->
            let acc, arg = find_common acc arg in acc, arg :: args) 
            (acc, []) args in
          acc, Gen_op (op, List.rev args)
        | _ -> failwith "Impossible" in
      let i = get_index ex in
      if i >= 0 then
        acc, mk_ref_var i
      else if get_count ex < 2 then
        acc, ex'
      else begin
        let i = List.length acc in
        set_index ex i;
        ex' :: acc, mk_ref_var i
      end
    in
    count ex;
    let acc, ex = find_common [] ex in
    List.rev (ex :: acc)