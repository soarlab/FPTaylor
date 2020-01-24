(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Data structures for loaded variables, definitions, and expressions         *)
(* -------------------------------------------------------------------------- *)

open Expr
open Rounding
open Eval
open Num
open Task
open Interval

type raw_expr =
  | Identifier of string
  | Numeral of num
  | Raw_rounding of rnd_info * raw_expr
  | Raw_u_op of string * raw_expr
  | Raw_bin_op of string * raw_expr * raw_expr
  | Raw_gen_op of string * raw_expr list

type raw_formula =
  | Raw_le of raw_expr * raw_expr
  | Raw_lt of raw_expr * raw_expr
  | Raw_eq of raw_expr * raw_expr

type constant_def = {
  const_name : string;
  value : Const.t;
}

type var_def = {
  var_type : value_type;
  var_name : string;
  lo_bound : Const.t;
  hi_bound : Const.t;
  uncertainty : Const.t;
}

type definition = {
  def_name : string;
  def_expr : expr;
}

type env = {
  constants : (string, constant_def) Hashtbl.t;
  variables : (string, var_def) Hashtbl.t;
  definitions : (string, definition) Hashtbl.t;
  mutable constraints : (string * formula) list;
  mutable expressions : (string * expr) list;
}

let env = {
  constants = Hashtbl.create 10;
  variables = Hashtbl.create 10;
  definitions = Hashtbl.create 10;
  constraints = [];
  expressions = [];
}

let rec fillrnd1 rnd expr = match expr with

  | Const _ -> Rounding (rnd, expr)
  | Var _ -> Rounding (rnd, expr)
  | Rounding (r,e) -> Rounding (r, fillrnd1 rnd e)
  | U_op (op, arg) -> Rounding (rnd, U_op (op, fillrnd1 rnd arg))
  | Bin_op (op, arg1, arg2) ->Rounding (rnd, Bin_op (op, fillrnd1 rnd arg1,  fillrnd1 rnd arg2))
  | Gen_op (op, args) -> Rounding(rnd, Gen_op (op,List.map (fillrnd1 rnd) args))

let getfirstrnd expr = match expr with | Rounding (rnd,expr) ->rnd 
 
let fillrnd expr = fillrnd1 (getfirstrnd expr) expr 

let reset () =
  let clear = Hashtbl.clear in
  clear env.constants;
  clear env.variables;
  clear env.definitions;
  env.constraints <- [];
  env.expressions <- []

let env_to_tasks () =
  let vars = Hashtbl.fold 
      (fun _ v vs -> {
           Task.var_name = v.var_name;
           var_type = v.var_type;
           lo_bound = v.lo_bound;
           hi_bound = v.hi_bound;
           uncertainty = v.uncertainty;
         } :: vs) 
      env.variables [] in
  let rec loop acc = function
    | [] -> acc
    | (name, e) :: es ->
      let t = {
        Task.name = name;
        expression = e;
        variables = vars;
        constraints = env.constraints;
      } in
      loop (t :: acc) es in
  List.rev (loop [] env.expressions)

let find_constant name =
  Hashtbl.find env.constants name

let find_variable name =
  Hashtbl.find env.variables name

let find_definition name =
  Hashtbl.find env.definitions name

let all_variables () =
  Hashtbl.fold (fun _ v s -> v :: s) env.variables []

let all_constraints () =
  env.constraints

let variable_interval name =
  let var = find_variable name in {
    low = (Const.to_interval var.lo_bound).low;
    high = (Const.to_interval var.hi_bound).high;
  }

let get_low_bound name =
  let v = find_variable name in
  v.lo_bound

let get_high_bound name =
  let v = find_variable name in
  v.hi_bound

let get_var_type name =
  let v = find_variable name in
  v.var_type

(* Applies a given rounding operation recursively *)
let rec apply_raw_rounding rnd expr =
  match expr with
  | Identifier _ -> Raw_rounding (rnd, expr)
  | Numeral _ -> Raw_rounding (rnd, expr)
  (* Do not apply the rounding inside another rounding and
     do not round twice *)
  | Raw_rounding _ -> expr
  | Raw_u_op (op, arg) -> 
    let e1 = apply_raw_rounding rnd arg in
    Raw_rounding (rnd, Raw_u_op (op, e1))
  | Raw_bin_op (op, arg1, arg2) ->
    let e1 = apply_raw_rounding rnd arg1 and
    e2 = apply_raw_rounding rnd arg2 in
    Raw_rounding (rnd, Raw_bin_op (op, e1, e2))
  | Raw_gen_op (op, args) ->
    let es = List.map (apply_raw_rounding rnd) args in
    Raw_rounding (rnd, Raw_gen_op (op, es))

let apply_raw_rounding_to_formula rnd f =
  match f with
  | Raw_le (e1, e2) ->
    Raw_le (apply_raw_rounding rnd e1, apply_raw_rounding rnd e2)
  | Raw_lt (e1, e2) ->
    Raw_lt (apply_raw_rounding rnd e1, apply_raw_rounding rnd e2)
  | Raw_eq (e1, e2) ->
    Raw_eq (apply_raw_rounding rnd e1, apply_raw_rounding rnd e2)


(* Builds an expression from a raw expression *)
let rec transform_raw_expr1 = function
  | Raw_rounding (rnd, arg) ->
    let e1 = transform_raw_expr1 arg in
    Rounding (rnd, e1)
  | Raw_u_op (str, arg) -> 
    let e1 = transform_raw_expr1 arg in
    begin
      match str with
      | "-" -> U_op (Op_neg, e1)
      | "abs" -> U_op (Op_abs, e1)
      | "inv" -> U_op (Op_inv, e1)
      | "sqrt" -> U_op (Op_sqrt, e1)
      | "sin" -> U_op (Op_sin, e1)
      | "cos" -> U_op (Op_cos, e1)
      | "tan" -> U_op (Op_tan, e1)
      | "asin" -> U_op (Op_asin, e1)
      | "acos" -> U_op (Op_acos, e1)
      | "atan" -> U_op (Op_atan, e1)
      | "exp" -> U_op (Op_exp, e1)
      | "log" -> U_op (Op_log, e1)
      | "sinh" -> U_op (Op_sinh, e1)
      | "cosh" -> U_op (Op_cosh, e1)
      | "tanh" -> U_op (Op_tanh, e1)
      | "relu" -> Bin_op (Op_max,mk_const (Const.of_float 0.0),e1)
      | "asinh" -> U_op (Op_asinh, e1)
      | "acosh" -> U_op (Op_acosh, e1)
      | "atanh" -> U_op (Op_atanh, e1)
      | "floor_power2" -> U_op (Op_floor_power2, e1)
      | _ -> failwith ("transform_raw_expr: Unknown unary operation: " ^ str)
    end
  | Raw_bin_op (str, arg1, arg2) -> 
    let e1 = transform_raw_expr1 arg1 and
    e2 = transform_raw_expr1 arg2 in
    begin
      match str with
      | "+" -> Bin_op (Op_add, e1, e2)
      | "-" -> Bin_op (Op_sub, e1, e2)
      | "*" -> Bin_op (Op_mul, e1, e2)
      | "/" -> Bin_op (Op_div, e1, e2)
      | "max" -> Bin_op (Op_max, e1, e2)
      | "min" -> Bin_op (Op_min, e1, e2)
      | "^" -> Bin_op (Op_nat_pow, e1, e2)
      | "sub2" -> Bin_op (Op_sub2, e1, e2)
      | _ -> failwith ("transform_raw_expr: Unknown binary operation: " ^ str)
    end
  | Raw_gen_op (str, args) ->
    let es = List.map transform_raw_expr1 args in
    begin
      match str with
      | "fma" -> Gen_op (Op_fma, es)
      | _ -> failwith ("transform_raw_expr: Unknown operation: " ^ str)
    end
  | Numeral n -> mk_num_const n
  | Identifier name ->
    try let def = find_definition name in def.def_expr
    with Not_found ->
    try let var = find_variable name in Var var.var_name
    with Not_found ->
      let c = find_constant name in Const c.value

let transform_raw_expr re = match re with |Raw_rounding _ -> fillrnd (transform_raw_expr1 re)  | _ -> transform_raw_expr1 re

(* Builds a formula from a raw formula *)
let transform_raw_formula = function
  | Raw_le (r1, r2) -> Le (transform_raw_expr r1, transform_raw_expr r2)
  | Raw_lt (r1, r2) -> Lt (transform_raw_expr r1, transform_raw_expr r2)
  | Raw_eq (r1, r2) -> Eq (transform_raw_expr r1, transform_raw_expr r2)

(* Adds a constant to the environment *)
let add_constant name raw =
  if Hashtbl.mem env.constants name then
    failwith ("Constant " ^ name ^ " is already defined")
  else
    let expr = transform_raw_expr raw in
    let c = {
      const_name = name;
      value = eval_const_expr expr;
    } in
    Hashtbl.add env.constants name c

let is_same_bounds lo hi =
  try
    let a = Const.to_num lo and
    b = Const.to_num hi in
    a =/ b
  with _ -> false

(* Adds a variable to the environment *)
let add_variable_with_uncertainty var_type name lo hi uncertainty =
  if Hashtbl.mem env.variables name then
    failwith ("Variable " ^ name ^ " is already defined")
  else
    let lo_expr = transform_raw_expr lo and
    hi_expr = transform_raw_expr hi and
    u_expr = transform_raw_expr uncertainty in
    let lo_bound = eval_const_expr lo_expr and
      hi_bound = eval_const_expr hi_expr in
    if var_type = real_type && is_same_bounds lo_bound hi_bound then
      begin
        Log.report `Info "Variable %s is a constant" name;
        add_constant name lo
      end
    else
      let v = {
        var_type = var_type;
        var_name = name;
        lo_bound = eval_const_expr lo_expr;
        hi_bound = eval_const_expr hi_expr;
        uncertainty = eval_const_expr u_expr;
      } in
      Hashtbl.add env.variables name v

let add_variable var_type name lo hi =
  add_variable_with_uncertainty var_type name lo hi (Numeral (Int 0))

(* Adds a definition to the environment *)
let add_definition name raw =
  if Hashtbl.mem env.definitions name then
    failwith ("Definition " ^ name ^ " is already defined")
  else
    let expr = transform_raw_expr raw in
    let d = {
      def_name = name;
      def_expr = expr;
    } in
    let _ = Hashtbl.add env.definitions name d in
    expr

(* Adds a constraint to the environment *)
let add_constraint name raw =
  let c = transform_raw_formula raw in
  env.constraints <- env.constraints @ [name, c]

(* Adds a named expression to the environment. Also creates the corresponding definition. *)
let add_expression_with_name name raw =
  let expr = add_definition name raw in
  env.expressions <- env.expressions @ [(name, expr)]

(* Adds an expression to the environment *)
let add_expression raw =
  let name = "Expression " ^ string_of_int (List.length env.expressions + 1) in
  add_expression_with_name name raw

(* Prints a raw expression *)
let print_raw_expr fmt =
  let p = Format.pp_print_string fmt in
  let rec print fmt = function
    | Identifier name -> p name
    | Numeral n -> p (string_of_num n)
    | Raw_rounding (rnd, e) ->
      Format.fprintf fmt "%s(%a)" (rounding_to_string rnd) print e;
    | Raw_u_op (op, e) -> 
      Format.fprintf fmt "%s(%a)" op print e;
    | Raw_bin_op (op, e1, e2) ->
      Format.fprintf fmt "%s(%a,%a)" op print e1 print e2;
    | Raw_gen_op (op, es) ->
      p op; p "("; Lib.print_list (print fmt) (fun () -> p ",") es; p ")";
  in
  print fmt

let print_raw_expr_std = print_raw_expr Format.std_formatter

let print_raw_formula fmt = function
  | Raw_le (e1, e2) -> 
    Format.fprintf fmt "%a <= %a\n" print_raw_expr e1 print_raw_expr e2
  | Raw_lt (e1, e2) -> 
    Format.fprintf fmt "%a < %a\n" print_raw_expr e1 print_raw_expr e2
  | Raw_eq (e1, e2) -> 
    Format.fprintf fmt "%a == %a\n" print_raw_expr e1 print_raw_expr e2

let print_raw_formula_std = print_raw_formula Format.std_formatter
