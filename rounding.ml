(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Parameters for rounding operations                                         *)
(* -------------------------------------------------------------------------- *)

(* significand_bits = min_exp = max_exp = max_int <=> a real number *)
(* significand_bits = 0 <=> no rounding *)
type value_type = {
  significand_bits : int;
  min_exp : int;
  max_exp : int;
}

type rnd_type = Rnd_ne | Rnd_up | Rnd_down | Rnd_0

type rnd_info = {
  (* Approximation of the maximum value *)
  max_val : float;
  eps_exp : int;
  delta_exp : int;
  coefficient : float;
  fp_type : value_type;
  rnd_type : rnd_type;
  special_flag : bool;
}

let mk_value_type bits (min_exp, max_exp) = {
  significand_bits = bits;
  min_exp = min_exp;
  max_exp = max_exp;
}

let value_type_of_bits sig_bits exp_bits =
  if exp_bits < 2 || exp_bits > 60 then failwith ("Bad value of exp_bits: " ^ string_of_int exp_bits);
  let max_exp = 1 lsl (exp_bits - 1) - 1 in
  mk_value_type sig_bits (1 - max_exp, max_exp)

let value_type_of_total_bits bits =
  let sig_bits, max_exp = 
    match bits with
    | 0 -> 0, 0
    | 16 -> 11, 15
    | 32 -> 24, 127
    | 64 -> 53, 1023
    | 128 -> 113, 16383
    | _ when bits = max_int -> max_int, max_int
    | _ -> failwith ("Unsupported fp size: " ^ string_of_int bits)
  in
  mk_value_type sig_bits (1 - max_exp, max_exp)

(* Returns true if type1 is a subtype of type2 *)
(* TODO: this test is not very precise; consider float<20, 2> and float<9, 3> *)
let is_subtype type1 type2 =
  type1.significand_bits <= type2.significand_bits &&
  type1.min_exp >= type2.min_exp &&
  type1.max_exp <= type2.max_exp

let is_no_rnd rnd =
  rnd.fp_type.significand_bits = 0

let eps_from_type t = -t.significand_bits

let delta_from_type t = -t.significand_bits + t.min_exp

let max_value_from_type t =
  if t.significand_bits = max_int then infinity
  else
    min (Fpu.fsub_low 2.0 (ldexp 1.0 (-t.significand_bits + 1)) *. ldexp 1.0 t.max_exp) max_float

(* Definitions for common types *)
let real_type = mk_value_type max_int (min_int, max_int)
let fp16_type = value_type_of_total_bits 16
let fp32_type = value_type_of_total_bits 32
let fp64_type = value_type_of_total_bits 64
let fp128_type = value_type_of_total_bits 128

(* TODO: this function is only used for producing proof traces; it should be removed *)
let type_to_bits t =
  match t.significand_bits with
  | 11 -> 16
  | 24 -> 32
  | 53 -> 64
  | 113 -> 128
  | _ -> failwith ("Cannot convert type to bits")

(* type_precision and type_min_exp are used in the ulp_errors function in fptaylor.ml *)
let type_precision t = t.significand_bits

let type_min_exp t = t.min_exp

let string_to_rnd_type str =
  match str with
    | "ne" | "nearest" -> false, Rnd_ne
    | "down" | "negative_infinity" -> true, Rnd_down
    | "up" | "positive_infinity" -> true, Rnd_up
    | "zero" | "toward_zero" -> true, Rnd_0
    | _ -> failwith ("Unknown rounding type: " ^ str)

let create_rounding t rnd c =
  let dir_flag, rnd_type = string_to_rnd_type rnd in {
    max_val = max_value_from_type t;
    eps_exp = eps_from_type t;
    delta_exp = delta_from_type t;
    coefficient = if dir_flag then 2.0 *. c else c;
    fp_type = t;
    rnd_type = rnd_type;
    special_flag = false;
  }

let create_explicit_rounding t rnd c eps delta = {
  max_val = max_value_from_type t;
  eps_exp = eps;
  delta_exp = delta;
  coefficient = c;
  fp_type = t;
  rnd_type = snd (string_to_rnd_type rnd);
  special_flag = false;
}

let value_type_table = [
  "float16", fp16_type;
  "float32", fp32_type;
  "float64", fp64_type;
  "float128", fp128_type;
  "real", real_type
]

let string_to_value_type str =
  try Lib.assoc str value_type_table
  with Not_found ->
    failwith ("Value type " ^ str ^ " is not defined")

let rounding_table = [
  (* 16 bit rounding *)
  "rnd16", create_rounding fp16_type "ne" 1.0;
  "rnd16_up", create_rounding fp16_type "up" 1.0;
  "rnd16_down", create_rounding fp16_type "down" 1.0;
  "rnd16_0", create_rounding fp16_type "zero" 1.0;
  (* 32 bit rounding *)
  "rnd32", create_rounding fp32_type "ne" 1.0;
  "rnd32_up", create_rounding fp32_type "up" 1.0;
  "rnd32_down", create_rounding fp32_type "down" 1.0;
  "rnd32_0", create_rounding fp32_type "zero" 1.0;
  (* 64 bit rounding *)
  "rnd64", create_rounding fp64_type "ne" 1.0;
  "rnd64_up", create_rounding fp64_type "up" 1.0;
  "rnd64_down", create_rounding fp64_type "down" 1.0;
  "rnd64_0", create_rounding fp64_type "zero" 1.0;
  (* 128 bit rounding *)
  "rnd128", create_rounding fp128_type "ne" 1.0;
  "rnd128_up", create_rounding fp128_type "up" 1.0;
  "rnd128_down", create_rounding fp128_type "down" 1.0;
  "rnd128_0", create_rounding fp128_type "zero" 1.0; 
]

let string_to_rounding name =
  try Lib.assoc name rounding_table
  with Not_found ->
    failwith ("Rounding mode " ^ name ^ " is not defined")

let rounding_type_to_string rnd_type = 
  match rnd_type with
    | Rnd_ne -> "ne"
    | Rnd_up -> "up"
    | Rnd_down -> "down"
    | Rnd_0 -> "zero"

let value_type_to_string t =
  try Lib.rev_assoc t value_type_table
  with Not_found ->
    Printf.sprintf "<%d,(%d,%d)>" t.significand_bits t.min_exp t.max_exp

let rounding_to_string rnd =
  try Lib.rev_assoc rnd rounding_table 
  with Not_found ->
    Printf.sprintf "rnd[%s,%s,%.2f,%d,%d]" 
      (value_type_to_string rnd.fp_type)
      (rounding_type_to_string rnd.rnd_type) 
      rnd.coefficient rnd.eps_exp rnd.delta_exp

let get_eps exp =
  if exp = 0 then 0.0
  else
    let f = ldexp 1.0 exp in
    if f = 0.0 then ldexp 1.0 (-52 - 1022) else f
