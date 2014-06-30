open Lib

(* bits = 0 <=> a real number *)
type value_type = {
  bits : int;
}

let mk_value_type bits = { bits = bits }

type rnd_type = Rnd_ne | Rnd_up | Rnd_down | Rnd_0

type rnd_info = {
  eps_exp : int;
  delta_exp : int;
  coefficient : float;
  fp_type : value_type;
  rnd_type : rnd_type;
}

let eps_delta_from_bits bits =
  match bits with
    | 16 -> -11, -14
    | 32 -> -24, -126
    | 64 -> -53, -1022
    | 128 -> -113, -16382
    | _ -> failwith ("Unsupported fp size: " ^ string_of_int bits)

let create_rounding bits rnd c =
  let fp_type = { bits = bits } in
  let dir_flag, rnd_type = 
    match rnd with
      | "ne" | "nearest" -> false, Rnd_ne
      | "down" | "negative_infinity" -> true, Rnd_down
      | "up" | "positive_infinity" -> true, Rnd_up
      | "0" | "toward_zero" -> true, Rnd_0
      | _ -> failwith ("Unknown rounding type: " ^ rnd) in
  let eps, delta = eps_delta_from_bits bits in {
    eps_exp = eps;
    delta_exp = delta;
    coefficient = if dir_flag then 2.0 *. c else c;
    fp_type = fp_type;
    rnd_type = rnd_type;
  }

let rounding_table = [
  (* 16 bit rounding *)
  "rnd16", create_rounding 16 "ne" 1.0;
  "rnd16_up", create_rounding 16 "up" 1.0;
  "rnd16_down", create_rounding 16 "down" 1.0;
  "rnd16_0", create_rounding 16 "0" 1.0;
  (* 32 bit rounding *)
  "rnd32", create_rounding 32 "ne" 1.0;
  "rnd32_up", create_rounding 32 "up" 1.0;
  "rnd32_down", create_rounding 32 "down" 1.0;
  "rnd32_0", create_rounding 32 "0" 1.0;
  (* 64 bit rounding *)
  "rnd64", create_rounding 64 "ne" 1.0;
  "rnd64_up", create_rounding 64 "up" 1.0;
  "rnd64_down", create_rounding 64 "down" 1.0;
  "rnd64_0", create_rounding 64 "0" 1.0;
  (* 128 bit rounding *)
  "rnd128", create_rounding 128 "ne" 1.0;
  "rnd128_up", create_rounding 128 "up" 1.0;
  "rnd128_down", create_rounding 128 "down" 1.0;
  "rnd128_0", create_rounding 128 "0" 1.0; 
]

let string_to_rounding name =
  try assoc name rounding_table
  with Failure _ ->
    failwith ("Rounding mode " ^ name ^ " is not defined")

let rounding_type_to_string rnd_type = 
  match rnd_type with
    | Rnd_ne -> "ne"
    | Rnd_up -> "up"
    | Rnd_down -> "down"
    | Rnd_0 -> "0"

let rounding_to_string rnd =
  try rev_assoc rnd rounding_table 
  with Failure _ ->
    Printf.sprintf "rnd(%d,%s,%f)" 
      rnd.fp_type.bits (rounding_type_to_string rnd.rnd_type) rnd.coefficient

let get_eps exp =
  if exp = 0 then 0.0
  else
    let f = ldexp 1.0 exp in
    if f = 0.0 then ldexp 1.0 (-52 - 1022) else f

