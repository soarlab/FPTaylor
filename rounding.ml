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

(* bits = max_int <=> a real number *)
type value_type = {
  bits : int;
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

let mk_value_type bits = { bits = bits }

let real_type = mk_value_type max_int

let string_to_value_type str =
  match str with
  | "float16" -> mk_value_type 16
  | "float32" -> mk_value_type 32
  | "float64" -> mk_value_type 64
  | "float128" -> mk_value_type 128
  | "real" -> real_type
  | _ -> failwith ("Unknown type: " ^ str)

(* Returns true if type1 is a subtype of type2 *)
let is_subtype type1 type2 =
  type1.bits <= type2.bits

let is_no_rnd rnd =
  rnd.fp_type.bits = 0

let get_precision rnd = rnd.fp_type.bits

let eps_delta_from_bits bits =
  match bits with
    | 0 -> 0, 0
    | 16 -> -11, -14
    | 32 -> -24, -126
    | 64 -> -53, -1022
    | 128 -> -113, -16382
    | _ -> failwith ("Unsupported fp size: " ^ string_of_int bits)

let max_value_from_bits bits =
  let p, emax =
    match bits with
      | 0 -> 0, 0
      | 16 -> 10, 15
      | 32 -> 23, 127
      | 64 -> 52, 1023
      | 128 -> 112, 16383 
      | _ -> failwith ("max_value_from_bits: Unsupported fp size: " ^ string_of_int bits) in
  (2.0 -. ldexp 1.0 (-p)) *. ldexp 1.0 emax

let string_to_rnd_type str =
  match str with
    | "ne" | "nearest" -> false, Rnd_ne
    | "down" | "negative_infinity" -> true, Rnd_down
    | "up" | "positive_infinity" -> true, Rnd_up
    | "zero" | "toward_zero" -> true, Rnd_0
    | _ -> failwith ("Unknown rounding type: " ^ str)

let create_rounding bits rnd c =
  let fp_type = { bits = bits } in
  let dir_flag, rnd_type = string_to_rnd_type rnd in
  let eps, delta = eps_delta_from_bits bits in {
    max_val = max_value_from_bits bits;
    eps_exp = eps;
    delta_exp = delta;
    coefficient = if dir_flag then 2.0 *. c else c;
    fp_type = fp_type;
    rnd_type = rnd_type;
    special_flag = false;
  }

let create_explicit_rounding bits rnd c eps delta = {
  max_val = max_value_from_bits bits;
  eps_exp = eps;
  delta_exp = delta;
  coefficient = c;
  fp_type = { bits = bits };
  rnd_type = snd (string_to_rnd_type rnd);
  special_flag = false;
}

let rounding_table = [
  (* 16 bit rounding *)
  "rnd16", create_rounding 16 "ne" 1.0;
  "rnd16_up", create_rounding 16 "up" 1.0;
  "rnd16_down", create_rounding 16 "down" 1.0;
  "rnd16_0", create_rounding 16 "zero" 1.0;
  (* 32 bit rounding *)
  "rnd32", create_rounding 32 "ne" 1.0;
  "rnd32_up", create_rounding 32 "up" 1.0;
  "rnd32_down", create_rounding 32 "down" 1.0;
  "rnd32_0", create_rounding 32 "zero" 1.0;
  (* 64 bit rounding *)
  "rnd64", create_rounding 64 "ne" 1.0;
  "rnd64_up", create_rounding 64 "up" 1.0;
  "rnd64_down", create_rounding 64 "down" 1.0;
  "rnd64_0", create_rounding 64 "zero" 1.0;
  (* 128 bit rounding *)
  "rnd128", create_rounding 128 "ne" 1.0;
  "rnd128_up", create_rounding 128 "up" 1.0;
  "rnd128_down", create_rounding 128 "down" 1.0;
  "rnd128_0", create_rounding 128 "zero" 1.0; 
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

let rounding_to_string rnd =
  try Lib.rev_assoc rnd rounding_table 
  with Not_found ->
    Printf.sprintf "rnd[%d,%s,%.2f,%d,%d]" 
      rnd.fp_type.bits (rounding_type_to_string rnd.rnd_type) 
      rnd.coefficient rnd.eps_exp rnd.delta_exp

let get_eps exp =
  if exp = 0 then 0.0
  else
    let f = ldexp 1.0 exp in
    if f = 0.0 then ldexp 1.0 (-52 - 1022) else f
